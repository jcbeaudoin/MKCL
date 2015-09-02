;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file provides a port the SBCL/CMUCL 'serve-event'
;; functionality to mkcl.  serve-event provides a lispy abstraction of
;; unix select(2) non-blocking IO (and potentially other variants such as
;; epoll).  It works with Unix-level file-descriptors, which can be
;; retrieved from the sockets module using the socket-file-descriptor
;; slot.
;;
;; As this file is based on SBCL's serve-event module it is being
;; released under the same (non) license as SBCL (i.e. public-domain).
;;
;; The original port was made by Steve Smith (tarkasteve@gmail.com)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (defun test-stdin ()
;;   (format t "DOING STDIN~%")
;;   (with-fd-handler (0 :input #'(lambda (fd) (declare (ignore fd))
;;                                        (format t "Got data~%")
;;                                        (read-char)))
;;     (loop ;; FIXME: End condition
;;        (format t "Entering serve-all-events...~%")(force-output)
;;        (serve-all-events 5)
;;        (format t "Events served~%"))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A more advanced example using sockets is available here:
;;
;;    http://haltcondition.net/?p=2232
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage "SERVE-EVENT"
  (:use "CL" "FFI" "UFFI")
  (:export "WITH-FD-HANDLER" "ADD-FD-HANDLER" "REMOVE-FD-HANDLER"
           "SERVE-EVENT" "SERVE-ALL-EVENTS"))
(in-package "SERVE-EVENT")

#+unix
(clines
 "#include <errno.h>"
 "#include <sys/select.h>"
 )
#+windows
(clines
 "#include <errno.h>"
 "#include <winsock2.h>")


(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-c-constants (&rest args)
    `(progn
       ,@(loop
            for (lisp-name c-name) on args by #'cddr
            collect `(defconstant ,lisp-name (c-inline () () :int ,c-name :one-liner t)))))
  (defmacro c-constant (name)
    `(c-inline () () :int ,name :one-liner t)))

(define-c-constants
    +eintr+ "EINTR")

(defstruct (handler
            (:constructor make-handler (direction descriptor function))
            (:copier nil))
  ;; Reading or writing...
  (direction nil :type (member :input :output))
  ;; File descriptor this handler is tied to.
  ;; FIXME: Should be based on FD_SETSIZE
  (descriptor 0)
  ;; Function to call.
  (function nil :type function))


(defvar *descriptor-handlers* nil
  ;;#!+sb-doc
  "List of all the currently active handlers for file descriptors")


;;; Add a new handler to *descriptor-handlers*.
(defun add-fd-handler (fd direction function)
  "Arrange to call FUNCTION whenever FD is usable. DIRECTION should be
  either :INPUT or :OUTPUT. The value returned should be passed to
  SYSTEM:REMOVE-FD-HANDLER when it is no longer needed."
  (unless (member direction '(:input :output))
    ;; FIXME: should be TYPE-ERROR?
    (error "Invalid direction ~S, must be either :INPUT or :OUTPUT" direction))
  (let ((handler (make-handler direction fd function)))
    (push handler *descriptor-handlers*)
    handler))

;;; Remove an old handler from *descriptor-handlers*.
(defun remove-fd-handler (handler)
  ;;#!+sb-doc
  "Removes HANDLER from the list of active handlers."
  (setf *descriptor-handlers*
        (delete handler *descriptor-handlers*)))

;;; Add the handler to *descriptor-handlers* for the duration of BODY.
(defmacro with-fd-handler ((fd direction function) &rest body)
  "Establish a handler with SYSTEM:ADD-FD-HANDLER for the duration of BODY.
   DIRECTION should be either :INPUT or :OUTPUT, FD is the file descriptor to
   use, and FUNCTION is the function to call whenever FD is usable."
  (let ((handler (gensym)))
    `(let (,handler)
       (unwind-protect
           (progn
             (setf ,handler (add-fd-handler ,fd ,direction ,function))
             ,@body)
         (when ,handler
           (remove-fd-handler ,handler))))))


(defmacro fd-zero(fdset)
  `(c-inline (,fdset) (:object) :void 
             "FD_ZERO((fd_set*)#0->foreign.data)"
             :one-liner t
             :side-effects t))

(defmacro fd-set (fd fdset)
  `(c-inline (,fd ,fdset) (:int :object) :void 
             "FD_SET(#0, (fd_set*)#1->foreign.data);"
             :one-liner t
             :side-effects t))

(defmacro fd-isset (fd fdset)
  `(c-inline (,fd ,fdset) (:int :object) :int 
             "FD_ISSET(#0, (fd_set*)#1->foreign.data)"
             :one-liner t
             :side-effects t))

(defun fdset-size ()
  (c-inline () () :int "sizeof(fd_set)" :one-liner t :side-effects nil))


(defun serve-event (&optional (seconds nil))
  "Receive pending events on all FD-STREAMS and dispatch to the appropriate
   handler functions. If timeout is specified, server will wait the specified
   time (in seconds) and then return, otherwise it will wait until something
   happens. Server returns T if something happened and NIL otherwise. Timeout
   0 means polling without waiting."

  ;; fd_set is an opaque typedef, so we can't declare it locally.
  ;; However we can fine out its size and allocate a char array of
  ;; the same size which can be used in its place.
  (let ((fsize (fdset-size)))
    (with-foreign-objects ((rfd `(:array :unsigned-char ,fsize))
                           (wfd `(:array :unsigned-char ,fsize)))
      (fd-zero rfd)
      (fd-zero wfd)

      (let ((maxfd 0))
        ;; Load the descriptors into the relevant set
        (dolist (handler *descriptor-handlers*)
          (let ((fd (handler-descriptor handler)))
            (ecase (handler-direction handler)
              (:input (fd-set fd rfd))
              (:output (fd-set fd wfd)))
            (when (> fd maxfd)
          (setf maxfd fd))))

        (multiple-value-bind (retval errno)
	    (if (null seconds)
		;; No timeout
		(c-inline (rfd      wfd    (1+ maxfd))
			  (:object :object :int) (values :int :int)
			  "{ @(return 0) = select(#2, (fd_set*)#0->foreign.data,
                                                      (fd_set*)#1->foreign.data,
                                                      NULL, NULL);
                             @(return 1) = errno; }"
			  :one-liner nil
			  :side-effects t)
		(c-inline (rfd      wfd    (1+ maxfd) seconds) 
			  (:object :object :int       :int) (values :int :int)
			  "{ struct timeval tv;
                                tv.tv_sec = #3;
                                tv.tv_usec = 0;
                                @(return 0) = select(#2, (fd_set*)#0->foreign.data,
                                                         (fd_set*)#1->foreign.data,
                                                         NULL, &tv);
                                @(return 1) = errno; }"
			  :one-liner nil
			  :side-effects t))

	  (cond ((zerop retval) 
		 nil)
		((minusp retval)
		 (if (= errno +eintr+)
		     ;; suppress EINTR
		     nil
		     ;; otherwise error
		     (error "Error during select")))
		((plusp retval)  
		 (dolist (handler *descriptor-handlers*)
		   (let ((fd (handler-descriptor handler)))
		     (if (plusp (ecase (handler-direction handler)
				  (:input (fd-isset fd rfd))
				  (:output (fd-isset fd wfd))))
			 (funcall (handler-function handler) 
				  (handler-descriptor handler)))))
		 t)))))))


;;; Wait for up to timeout seconds for an event to happen. Make sure all
;;; pending events are processed before returning.
(defun serve-all-events (&optional (timeout nil))
  "SERVE-ALL-EVENTS calls SERVE-EVENT with the specified timeout. If
SERVE-EVENT does something (returns T) it loops over SERVE-EVENT with a
timeout of 0 until there are no more events to serve. SERVE-ALL-EVENTS returns
T if SERVE-EVENT did something and NIL if not."
  (do ((res nil)
       (sval (serve-event timeout) (serve-event 0)))
      ((null sval) res)
    (setq res t)))

(provide 'serve-event)
