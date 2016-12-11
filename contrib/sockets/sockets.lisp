;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; $Id$

;; This file is based on SBCL's SB-BSD-SOCKET module and has been
;; heavily modified to work with ECL by Julian Stecklina.
;; Port to Windows Sockets contributed by M. Goffioul.

;; Some rework to better fit with MKCL done by Jean-Claude Beaudoin in 2011-2012.
;; This implementation is still IPv4 only. JCB 2012/07/10.

;; You may do whatever you want with this file. (PUBLIC DOMAIN)

;; Trivial stuff is copied from SBCL's SB-BSD-SOCKETS, which is also
;; in the public domain.

;; (terpri)
;; (princ ";;; Loading sockets.lisp")
;; (finish-output)

(in-package "SB-BSD-SOCKETS")

;; Obviously this requires in one form or another some BSD compatible
;; socket interface.

#|
#+windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (push :wsock *features*))
|#

;; Include the neccessary headers
#-:windows
(clines
 "#include <sys/types.h>"
 "#include <sys/socket.h>"
 "#include <sys/un.h>"
 "#include <sys/time.h>"
 "#include <netdb.h>"
 "#include <string.h>"
 "#include <unistd.h>"
 "#include <netinet/in.h>"
 "#include <netinet/tcp.h>"
 "#include <errno.h>"
 "#include <fcntl.h>"
 )
#+:windows
(clines
 ;; "#include <winsock2.h>" ;; moved to mkcl.h just above windows.h, JCB
 "#include <errno.h>"
 "#include <fcntl.h>"
 ;;"#include <stdio.h>" ;; what for? JCB
 #-(or mingw32 mingw64)
 "typedef int ssize_t;"
 "typedef int socklen_t;"
 )

#+:windows
(progn
  (defvar +wsock-initialized+ nil)
  (defun wsock-initialize ()
    (unless +wsock-initialized+
      (if (c-inline () () :object
        "
{
	WSADATA wsadata;
	mkcl_object output;
        MKCL_LIBC_NO_INTR(env, (output = ((WSAStartup(MAKEWORD(2,2), &wsadata) == NO_ERROR) ? mk_cl_Ct : mk_cl_Cnil)));
	@(return) = output;
}")
	  (setf +wsock-initialized+ t)
	(error "Unable to initialize Windows Socket library"))))
  (wsock-initialize)
); #+:windows

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-c-constants (&rest args)
    `(progn
       ,@(loop
	    for (lisp-name c-name) on args by #'cddr
	    collect `(defparameter ,lisp-name (c-inline () () :int ,c-name :one-liner t)))))
  (defmacro c-constant (name)
    `(c-inline () () :int ,name :one-liner t)))

#+:windows
(Clines
 "#define AF_LOCAL -1"
)

(define-c-constants
  +af-inet+ "AF_INET"
  +af-local+ "AF_LOCAL"
  +eagain+ "EAGAIN"
  +eintr+ "EINTR")

#+:windows
(defconstant +af-named-pipe+ -2)

;; Foreign functions

(defentry ff-socket (:int :int :int) (:int "socket") :no-interrupts t)
(defentry ff-listen (:int :int) (:int "listen") :no-interrupts t)
(defentry ff-close (:int) (:int "close") :no-interrupts t)
#+:windows (defentry ff-closesocket (:int) (:int "closesocket") :no-interrupts t)

;;; This courtesy of Pierre Mai in comp.lang.lisp 08 Jan 1999 00:51:44 +0100
;;; Message-ID: <87lnjebq0f.fsf@orion.dent.isdn.cs.tu-berlin.de>

(defun split (string &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
The whitespace is elided from the result.  The whole string will be
split, unless `max' is a non-negative integer, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (loop for start = (position-if-not #'is-ws string)
          then (position-if-not #'is-ws string :start index)
          for index = (when #| and |# start
                           (if (and max (= (1+ word-count) max))
                               nil
                             (position-if #'is-ws string :start start)))
          while start
          collect (subseq string start index)
          count 1 into word-count
          while index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NAME RESOLVING
;;;

(defclass host-ent ()
  ((name :initarg :name :accessor host-ent-name)
   (aliases :initarg :aliases :accessor host-ent-aliases)
   (address-type :initarg :type :accessor host-ent-address-type)
					; presently always AF_INET
   (addresses :initarg :addresses :accessor host-ent-addresses))
  (:documentation ""))

(defgeneric host-ent-address (host-ent)
  (:documentation ""))

(defmethod host-ent-address ((host-ent host-ent))
  (car (host-ent-addresses host-ent)))

;; FIXME: Do we need to free the hostent structures?
;; FIXME: gethostbyname is NOT thread-safe nor re-entrant!
(defun get-host-by-name (host-name)
  "Returns a HOST-ENT instance for HOST-NAME or throws some kind of condition.
HOST-NAME may also be an IP address in dotted quad notation or some other
weird stuff - see gethostbyname(3) for grisly details."
  (let ((host-ent (make-instance 'host-ent)))
    (if (c-inline (host-name host-ent
			     #'(setf host-ent-name)
			     #'(setf host-ent-aliases)
			     #'(setf host-ent-address-type)
			     #'(setf host-ent-addresses))
		  (:cstring t t t t t) t
		  "
{
	struct hostent *hostent = gethostbyname(#0);

	if (hostent != NULL) {
 	        char **aliases;
                char **addrs;
                mkcl_object aliases_list = mk_cl_Cnil;
                mkcl_object addr_list = mk_cl_Cnil;
                int length = hostent->h_length;

		mkcl_funcall2(env, #2, mkcl_make_base_string_copy(env, hostent->h_name), #1);
                mkcl_funcall2(env, #4, mkcl_make_integer(env, hostent->h_addrtype), #1);

                for (aliases = hostent->h_aliases; *aliases != NULL; aliases++) {
                        aliases_list = MKCL_CONS(env, mkcl_make_base_string_copy(env, *aliases),aliases_list);
                }
                mkcl_funcall2(env, #3, aliases_list, #1);
                for (addrs = hostent->h_addr_list; *addrs != NULL; addrs++) {
                        mkcl_index pos;
                        mkcl_object vector = mkcl_funcall1(env, @make-array, MKCL_MAKE_FIXNUM(length));
                        for (pos = 0; pos < length; pos++)
                                mkcl_aset_index(env, vector, pos, MKCL_MAKE_FIXNUM((unsigned char)((*addrs)[pos])));
                        addr_list = MKCL_CONS(env, vector, addr_list);


                }
                mkcl_funcall2(env, #5, addr_list, #1);
                @(return) = #1;
	} else {
		@(return) = mk_cl_Cnil;
	}
}"
		  :side-effects t)
	host-ent
	(name-service-error "get-host-by-name"))))

;; FIXME: gethostbyaddr is NOT thread-safe nor re-entrant!
(defun get-host-by-address (address)
  (assert (and (typep address 'vector) (= (length address) 4)))
  (let ((host-ent (make-instance 'host-ent)))
    (if
     (c-inline (address host-ent
			#'(setf host-ent-name)
			#'(setf host-ent-aliases)
			#'(setf host-ent-address-type)
			#'(setf host-ent-addresses))
	       (t t t t t t) t
	       "
{
	unsigned char in_addr_vector[4];
	struct hostent *hostent;
	in_addr_vector[0] = mkcl_integer_to_word(env, mkcl_aref_index(env, #0,0));
	in_addr_vector[1] = mkcl_integer_to_word(env, mkcl_aref_index(env, #0,1));
	in_addr_vector[2] = mkcl_integer_to_word(env, mkcl_aref_index(env, #0,2));
	in_addr_vector[3] = mkcl_integer_to_word(env, mkcl_aref_index(env, #0,3));
        MKCL_LIBC_NO_INTR(env, (hostent = gethostbyaddr(((void *) in_addr_vector),4,AF_INET)));
	if (hostent != NULL) {
 	        char **aliases;
                char **addrs;
                mkcl_object aliases_list = mk_cl_Cnil;
                mkcl_object addr_list = mk_cl_Cnil;
                int length = hostent->h_length;

		mkcl_funcall2(env, #2, mkcl_make_base_string_copy(env, hostent->h_name), #1);
                mkcl_funcall2(env, #4, mkcl_make_integer(env, hostent->h_addrtype), #1);

                for (aliases = hostent->h_aliases; *aliases != NULL; aliases++) {
                        aliases_list = MKCL_CONS(env, mkcl_make_base_string_copy(env, *aliases),aliases_list);
                }
                mkcl_funcall2(env, #3, aliases_list, #1);

                for (addrs = hostent->h_addr_list; *addrs != NULL; addrs++) {
                        mkcl_index pos;
                        mkcl_object vector = mkcl_funcall1(env, @make-array, MKCL_MAKE_FIXNUM(length));

                        for (pos = 0; pos < length; pos++)
                                mkcl_aset_index(env, vector, pos, MKCL_MAKE_FIXNUM((unsigned char)((*addrs)[pos])));
                        addr_list = MKCL_CONS(env, vector, addr_list);


                }
                mkcl_funcall2(env, #5, addr_list, #1);
                @(return) = #1;
	} else {
		@(return) = mk_cl_Cnil;
	}
}"
	       :side-effects t)
     host-ent
     (name-service-error "get-host-by-address"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SOCKET BASE CLASS AND GENERIC FUNCTIONS
;;;

(defclass socket ()
  ((file-descriptor :initarg :descriptor
		    :reader socket-file-descriptor)
   (family :initform (error "No socket family")
	   :reader socket-family)
   (protocol :initarg :protocol
	     :reader socket-protocol
	     :documentation "Protocol used by the socket. If a
keyword, the symbol-name of the keyword will be passed to
GET-PROTOCOL-BY-NAME downcased, and the returned value used as
protocol. Other values are used as-is.")
   (type  :initarg :type
	  :reader socket-type
	  :initform :stream
	  :documentation "Type of the socket: :STREAM or :DATAGRAM.")
   (stream)
   #+:windows
   (non-blocking-p :initform nil)
   )
  (:documentation "Common base class of all sockets, not meant to be directly instantiated."))


(defmethod print-object ((object socket) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ "descriptor " stream)
    (ignore-errors (princ (slot-value object 'file-descriptor) stream))))

(defmethod shared-initialize :after ((socket socket) slot-names
				     &key protocol type
				     &allow-other-keys)
  (declare (ignore slot-names))
  (let* ((proto-num
	  (cond ((and protocol (keywordp protocol))
		 (get-protocol-by-name (string-downcase (symbol-name protocol))))
		(protocol protocol)
		(t 0)))
	 (fd (or (and (slot-boundp socket 'file-descriptor)
		      (socket-file-descriptor socket))
		 #+:windows
		 (and (member (socket-family socket) (list +af-named-pipe+ +af-local+)) 0)
		 (ff-socket (socket-family socket)
			    (ecase (or type
				       (socket-type socket))
			      ((:datagram) (c-constant "SOCK_DGRAM"))
			      ((:stream) (c-constant "SOCK_STREAM")))
			    proto-num))))
    (if (= fd -1) (socket-error "socket"))
    (setf (slot-value socket 'file-descriptor) fd
	  (slot-value socket 'protocol) proto-num)
    #+ ignore
    (sb-ext:finalize socket (lambda () (sockint::close fd)))))

;; Generics

(defgeneric socket-bind (socket &rest address)
  (:documentation "Bind SOCKET to ADDRESS, which may vary according to
socket family.  For the INET family, pass ADDRESS and PORT as two
arguments; for FILE address family sockets, pass the filename string.
See also bind(2)"))

(defgeneric socket-accept (socket)
  (:documentation "Perform the accept(2) call, returning a
newly-created connected socket and the peer address as multiple
values"))

(defgeneric socket-connect (socket &rest address)
  (:documentation "Perform the connect(2) call to connect SOCKET to a
  remote PEER.  No useful return value."))

(defgeneric socket-peername (socket)
  (:documentation "Return the socket's peer; depending on the address
  family this may return multiple values"))

(defgeneric socket-name (socket)
  (:documentation "Return the address (as vector of bytes) and port
  that the socket is bound to, as multiple values."))

(defgeneric socket-listen (socket backlog)
  (:documentation "Mark SOCKET as willing to accept incoming connections.  BACKLOG
defines the maximum length that the queue of pending connections may
grow to before new connection attempts are refused.  See also listen(2)"))

(defgeneric socket-receive (socket buffer length
			    &key
			    oob peek waitall element-type)
  (:documentation "Read LENGTH octets from SOCKET into BUFFER (or a freshly-consed buffer if
NIL), using recvfrom(2).  If LENGTH is NIL, the length of BUFFER is
used, so at least one of these two arguments must be non-NIL.  If
BUFFER is supplied, it had better be of an element type one octet wide.
Returns the buffer, its length, and the address of the peer
that sent it, as multiple values.  On datagram sockets, sets MSG_TRUNC
so that the actual packet length is returned even if the buffer was too
small"))

(defgeneric socket-send (socket buffer length 
			 &key 
                         address external-format oob eor dontroute dontwait 
			 nosignal confirm more)
  (:documentation "Send length octets from buffer into socket, using sendto(2).
If buffer is a string, it will converted to octets according to external-format&
If length is nil, the length of the octet buffer is used. The format of address
depends on the socket type (for example for inet domain sockets it would be a 
list of an ip address and a port). If no socket address is provided, send(2) 
will be called instead. Returns the number of octets written."))


(defgeneric socket-close (socket)
  (:documentation "Close SOCKET.  May throw any kind of error that write(2) would have
thrown.  If SOCKET-MAKE-STREAM has been called, calls CLOSE on that
stream instead"))

(defgeneric socket-make-stream (socket  &rest args)
    (:documentation "Find or create a STREAM that can be used for IO
on SOCKET (which must be connected).  ARGS are passed onto
SB-SYS:MAKE-FD-STREAM."))

(defgeneric non-blocking-mode (socket)
  (:documentation "Is SOCKET in non-blocking mode?"))

(defgeneric (setf non-blocking-mode) (non-blocking-p socket)
  (:documentation "Put SOCKET in non-blocking mode - or not, according to NON-BLOCKING-P"))

(defgeneric socket-close-low-level (socket)
  (:documentation "Close SOCKET at low level. Do not use directly."))

;; Methods

(defmethod socket-listen ((socket socket) backlog)
  (let ((r (ff-listen (socket-file-descriptor socket) backlog)))
    (if (= r -1)
        (socket-error "listen"))))

(defmethod socket-close-low-level ((socket socket))
  (ff-close (socket-file-descriptor socket)))

(defmethod socket-close ((socket socket))
  ;; note that if you have a socket _and_ a stream on the same fd,
  ;; the socket will avoid doing anything to close the fd in case
  ;; the stream has done it already - if so, it may have been
  ;; reassigned to some other file, and closing it would be bad

  (let ((fd (socket-file-descriptor socket)))
    (unless (eql fd -1) ; already closed
      (cond ((slot-boundp socket 'stream)
	     (close (slot-value socket 'stream)) ;; closes fd indirectly
	     (slot-makunbound socket 'stream))
	    ((= (socket-close-low-level socket) -1)
	     (socket-error "close")))
      (setf (slot-value socket 'file-descriptor) -1))))

(ffi::clines "
static void *
raw_buffer_pointer(MKCL, mkcl_object x, mkcl_index size)
{
	mkcl_type t = mkcl_type_of(x);
	int ok = 0;
	if (t == mkcl_t_base_string) {
		ok = (size <= x->base_string.dim);
	} else if (t == mkcl_t_vector) {
		mkcl_elttype aet = x->vector.elttype;
		if (aet == mkcl_aet_b8 || aet == mkcl_aet_i8 || aet == mkcl_aet_bc) {
			ok = (size <= x->vector.dim);
		} else if (aet == mkcl_aet_word || aet == mkcl_aet_index) {
			mkcl_index divisor = sizeof(mkcl_index);
			size = (size + divisor - 1) / divisor;
			ok = (size <= x->vector.dim);
		}
	}
	if (!ok) {
		mkcl_FEerror(env, \"Lisp object is not a valid socket buffer: ~A\", 1, x);
	}
	return (void *)x->vector.self.t;
}
")

#-:windows
;; FIXME: How bad is manipulating fillp directly?
(defmethod socket-receive ((socket socket) buffer length
			   &key oob peek waitall element-type)
  (unless (or buffer length) (error "You have to supply either buffer or length!"))
  (let ((buffer (or buffer (make-array length :element-type element-type)))
	(length (or length (length buffer)))
	(fd (socket-file-descriptor socket)))

    (multiple-value-bind (len-recv errno)
	   (c-inline (fd buffer length oob peek waitall :io)
		     (:int :object :int :bool :bool :bool :object)
                  (values :long :int)
		     "
{
        int flags = ( #3 ? MSG_OOB : 0 )  |
                    ( #4 ? MSG_PEEK : 0 ) |
                    ( #5 ? MSG_WAITALL : 0 );
        mkcl_type type = mkcl_type_of(#1);
	ssize_t len;

        MKCL_LIBC_Zzz(env, #6, (len = recvfrom(#0, raw_buffer_pointer(env, #1, #2), #2, flags, NULL, NULL)));
        mk_mt_test_for_thread_shutdown(env);
        if (len >= 0) {
               if (type == mkcl_t_vector) { #1->vector.fillp = len; }
               else if (type == mkcl_t_base_string) { #1->base_string.fillp = len; }
        }
        @(return 0) = len;
        @(return 1) = errno;
}
"
                  :one-liner nil)
      (cond ((and (= len-recv -1)
		  (or (eql errno +eagain+) (eql errno +eintr+))
		  )
             nil)
            ((= len-recv -1)
             (socket-error "receive"))
            (t 
             (values buffer len-recv))))))

#+:windows
(Clines
" /* This callback is used as IO Completion routine by WSARecvFrom and WSASend and WSASendTo here below. */
static void CALLBACK _socket_recv_io_done(DWORD dwError, DWORD cbTransferred, LPWSAOVERLAPPED lpOverlapped, DWORD dwFlags)
{
#if 0
  lpOverlapped->hEvent = (HANDLE) (uintptr_t) cbTransferred;
#endif
}
static void CALLBACK _socket_send_io_done(DWORD dwError, DWORD cbTransferred, LPWSAOVERLAPPED lpOverlapped, DWORD dwFlags)
{
#if 0
  lpOverlapped->hEvent = (HANDLE) (uintptr_t) cbTransferred;
#endif
}
"
)

#+:windows
;; FIXME: How bad is manipulating fillp directly?
(defmethod socket-receive ((socket socket) buffer length
			   &key oob peek waitall element-type)
  (declare (ignore waitall))
  (unless (or buffer length) (error "You have to supply either buffer or length!"))
  (let ((buffer (or buffer (make-array length :element-type element-type)))
	(length (or length (length buffer)))
	(fd (socket-file-descriptor socket)))

    (multiple-value-bind (len-recv)
	   (c-inline (fd buffer length oob peek :io)
		     (:int :object :int :bool :bool :object)
                  (values :long :int)
		     "
{
        int flags = ( #3 ? MSG_OOB : 0 ) | ( #4 ? MSG_PEEK : 0 );
        mkcl_type type = mkcl_type_of(#1);
	ssize_t len;

      SOCKET s = (SOCKET) #0;
      int rc;
      BOOL ok; 
      WSABUF DataBuf = { #2, raw_buffer_pointer(env, #1, #2) };
      DWORD BytesRecv = 0;
      DWORD Flags = flags;
      struct sockaddr_in From;
      int FromLen = sizeof(From);
      WSAOVERLAPPED RecvOverlapped = { 0 };

      MKCL_LIBC_NO_INTR(env, rc = WSARecvFrom(s, &DataBuf, 1, &BytesRecv, &Flags, (SOCKADDR *) &From, &FromLen, &RecvOverlapped, _socket_recv_io_done));
      if (rc == 0)
	{
	  DWORD wait_val;

          MKCL_LIBC_Zzz(env, #5, wait_val = SleepEx(0, TRUE));

	  if (wait_val != WAIT_IO_COMPLETION)
	    mkcl_FEwin32_error(env, \"WSARecvFrom() (sockets.lisp) failed to complete properly on socket\", 0);

	  mk_mt_test_for_thread_shutdown(env);

          len = BytesRecv;
        }
      else if (rc == SOCKET_ERROR)
	{
	  DWORD wait_val;

	  if (WSAGetLastError() != WSA_IO_PENDING)
            { @(return 0) = -1; goto _MKCL_RECEIVE_ERROR; }

	  do {
	    MKCL_LIBC_Zzz(env, #5, wait_val = SleepEx(INFINITE, TRUE));
	  } while ((wait_val == WAIT_IO_COMPLETION)
		   && (WSAGetOverlappedResult(s, &RecvOverlapped, &BytesRecv, FALSE, &Flags)
		       ? FALSE
		       : ((WSAGetLastError() == WSA_IO_INCOMPLETE)
			  ? TRUE
			  : (mkcl_FEwin32_error(env, \"WSAGetOverlappedResult() (sockets.lisp) failed unexpectedtly after WSARecv() on socket\", 0), FALSE))));
	  if (wait_val != WAIT_IO_COMPLETION)
	    mkcl_FEwin32_error(env, \"WSARecvFrom() (sockets.lisp) failed to properly complete deferred IO on socket\", 0);

	  mk_mt_test_for_thread_shutdown(env);

          len = BytesRecv;
	}
      else
        { @(return 0) = -1; goto _MKCL_RECEIVE_ERROR; }

        if (len >= 0) {
               if (type == mkcl_t_vector) { #1->vector.fillp = len; }
               else if (type == mkcl_t_base_string) { #1->base_string.fillp = len; }
        }
        @(return 0) = len;
_MKCL_RECEIVE_ERROR:;
}
"
                  :one-liner nil)
      (cond ((= len-recv -1)
             (socket-error "receive"))
            (t 
             (values buffer len-recv))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INET SOCKETS
;;;

;; We could refactor a lot here, if we pass sockaddr_foo structs around in Lisp. But
;; I do not feel comfortable with that.

(defun get-protocol-by-name (string-or-symbol)
  "Calls getprotobyname"
  (let ((string (string string-or-symbol)))
    (c-inline (string) (:cstring) :int
	      "getprotobyname(#0)->p_proto"
	      :one-liner t)))

(defun make-inet-address (dotted-quads)
  "Return a vector of octets given a string DOTTED-QUADS in the format
\"127.0.0.1\""
  (map 'vector
       #'parse-integer
       (split dotted-quads nil '(#\.))))

(defclass inet-socket (socket)
  ((family :initform +af-inet+))
  (:documentation "Class representing TCP and UDP sockets.

Examples:

 (make-instance 'inet-socket :type :stream :protocol :tcp)

 (make-instance 'inet-socket :type :datagram :protocol :udp)
"))

(defun make-inet-socket (type protocol)
  "Make an INET socket.  Deprecated in favour of make-instance"
  (make-instance 'inet-socket :type type :protocol protocol))

(Clines
 "
static void fill_inet_sockaddr(struct sockaddr_in *sockaddr, int port,
			       int a1, int a2, int a3, int a4)
{
#ifdef MKCL_WINDOWS
	memset(sockaddr,0,sizeof(struct sockaddr_in));
#else
	bzero(sockaddr,sizeof(struct sockaddr_in));
#endif
	sockaddr->sin_family = AF_INET;
	sockaddr->sin_port = htons(port);
	sockaddr->sin_addr.s_addr= htonl((uint32_t)a1<<24 | (uint32_t)a2<<16 | (uint32_t)a3<<8 | (uint32_t)a4) ;

}
")



(defmethod socket-bind ((socket inet-socket) &rest address)
  (assert (= 2 (length address)) (address) "Socket-bind needs three parameters for inet sockets.")
  (let ((ip (first address))
	(port (second address)))
    (if (= -1
	   (c-inline (port (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)
			   (socket-file-descriptor socket))
		     (:int :int :int :int :int :int)
		     :int
		     "
{
	struct sockaddr_in sockaddr;
	int output;

        MKCL_LIBC_NO_INTR(env, fill_inet_sockaddr(&sockaddr, #0, #1, #2, #3, #4));
        MKCL_LIBC_NO_INTR(env, (output = bind(#5, (struct sockaddr*)&sockaddr, sizeof(struct sockaddr_in))));
	@(return) = output;
}"
		     :side-effects t))
	(socket-error "bind"))))

(defmethod socket-accept ((socket inet-socket))
  (let ((sfd (socket-file-descriptor socket)))
    (multiple-value-bind (fd vector port)
      (c-inline (sfd :io) (:int :object) (values :int :object :int)
"{
        struct sockaddr_in sockaddr;
        socklen_t addr_len = (socklen_t)sizeof(struct sockaddr_in);
        int new_fd;

        MKCL_LIBC_Zzz(env, #1, (new_fd = accept(#0, (struct sockaddr*)&sockaddr, &addr_len)));
        mk_mt_test_for_thread_shutdown(env);

	@(return 0) = new_fd;
	@(return 1) = mk_cl_Cnil;
        @(return 2) = 0;
        if (new_fd != -1) {
                uint32_t ip = ntohl(sockaddr.sin_addr.s_addr);
                uint16_t port = ntohs(sockaddr.sin_port);
                mkcl_object vector = mk_cl_make_array(env, 1,MKCL_MAKE_FIXNUM(4));

                mkcl_aset_index(env, vector,0, MKCL_MAKE_FIXNUM( ip>>24 ));
		mkcl_aset_index(env, vector,1, MKCL_MAKE_FIXNUM( (ip>>16) & 0xFF));
		mkcl_aset_index(env, vector,2, MKCL_MAKE_FIXNUM( (ip>>8) & 0xFF));
                mkcl_aset_index(env, vector,3, MKCL_MAKE_FIXNUM( ip & 0xFF ));

		@(return 1) = vector;
                @(return 2) = port;
	}
}") ;; FIXME: On Windows this code should be using AcceptEx with an alertable wait. JCB
      (cond
	((= fd -1)
	 (socket-error "accept"))
	(t
	 (values
	   (make-instance (class-of socket)
			  :type (socket-type socket)
			  :protocol (socket-protocol socket)
			  :descriptor fd)
	   vector
	   port))))))

(defmethod socket-connect ((socket inet-socket) &rest address)
  (let ((ip (first address))
	(port (second address)))
    (if (= -1
	   (c-inline (port (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)
			   (socket-file-descriptor socket) :io)
		     (:int :int :int :int :int :int :object)
		     :int
		     "
{
	struct sockaddr_in sockaddr;
	int output;

	MKCL_LIBC_NO_INTR(env, fill_inet_sockaddr(&sockaddr, #0, #1, #2, #3, #4));
	MKCL_LIBC_Zzz(env, #6, (output = connect(#5,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_in))));
        mk_mt_test_for_thread_shutdown(env);

	@(return) = output;
}")) ;; FIXME: On Windows this code should use connectEx with an alertable wait. JCB
	(socket-error "connect"))))

(defmethod socket-peername ((socket inet-socket))
  (let* ((vector (make-array 4))
	 (fd (socket-file-descriptor socket))
	 (port (c-inline (fd vector) (:int t) :int
"@01;{
        struct sockaddr_in name;
        socklen_t len = sizeof(struct sockaddr_in);
        int ret;

	MKCL_LIBC_NO_INTR(env, (ret = getpeername(#0,(struct sockaddr*)&name,&len)));
        if (ret == 0) {
                uint32_t ip = ntohl(name.sin_addr.s_addr);
                uint16_t port = ntohs(name.sin_port);

                mkcl_aset_index(env, #1,0, MKCL_MAKE_FIXNUM( ip>>24 ));
		mkcl_aset_index(env, #1,1, MKCL_MAKE_FIXNUM( (ip>>16) & 0xFF));
		mkcl_aset_index(env, #1,2, MKCL_MAKE_FIXNUM( (ip>>8) & 0xFF));
                mkcl_aset_index(env, #1,3, MKCL_MAKE_FIXNUM( ip & 0xFF ));

                @(return) = port;
         } else {
                @(return) = -1;
         }
}")))
    (if (>= port 0)
	(values vector port)
	(socket-error "getpeername"))))

(defmethod socket-name ((socket inet-socket))
  (let* ((vector (make-array 4))
	 (fd (socket-file-descriptor socket))
	 (port (c-inline (fd vector) (:int t) :int
"@01;{
        struct sockaddr_in name;
        socklen_t len = sizeof(struct sockaddr_in);
        int ret;

	MKCL_LIBC_NO_INTR(env, (ret = getsockname(#0,(struct sockaddr*)&name,&len)));

        if (ret == 0) {
                uint32_t ip = ntohl(name.sin_addr.s_addr);
                uint16_t port = ntohs(name.sin_port);

                mkcl_aset_index(env, #1,0, MKCL_MAKE_FIXNUM( ip>>24 ));
		mkcl_aset_index(env, #1,1, MKCL_MAKE_FIXNUM( (ip>>16) & 0xFF));
		mkcl_aset_index(env, #1,2, MKCL_MAKE_FIXNUM( (ip>>8) & 0xFF));
                mkcl_aset_index(env, #1,3, MKCL_MAKE_FIXNUM( ip & 0xFF ));

                @(return) = port;
         } else {
                @(return) = -1;
         }
}")))
    (if (>= port 0)
	(values vector port)
	(socket-error "getsockname"))))


#-:windows
(defmethod socket-send ((socket socket) buffer length
			   &key address external-format oob eor dontroute dontwait nosignal #+linux confirm more)
  (declare (ignore external-format more))
  (assert (or (stringp buffer)
		(typep buffer 'vector)))
  (let (;eh, here goes string->octet convertion... 
	;When will ecl support Unicode?
	(length (or length (length buffer)))
	(fd (socket-file-descriptor socket)))
    (let ((len-sent
	   (if address
	       (progn
		 (assert (= 2 (length address)))
		 (c-inline (fd buffer length 
			       (second address)
			       (aref (first address) 0)
			       (aref (first address) 1)
			       (aref (first address) 2)
			       (aref (first address) 3)
                               :io
			       oob eor dontroute dontwait nosignal
			       #+linux confirm #-linux nil
			       )
		     (:int :object :int
			   :int :int :int :int :int
                           :object
			   :bool :bool :bool :bool :bool :bool
                           )
		     :long
		     "
{
        int flags = ( #9 ? MSG_OOB : 0 )  |
                    ( #a ? MSG_EOR : 0 ) |
                    ( #b ? MSG_DONTROUTE : 0 ) |
                    ( #c ? MSG_DONTWAIT : 0 ) |
                    ( #d ? MSG_NOSIGNAL : 0 )
#if __linux
                    | ( #e ? MSG_CONFIRM : 0 )
#endif
                    ;
        mkcl_type type = mkcl_type_of(#1);
        struct sockaddr_in sockaddr;
	ssize_t len = 0;
        mkcl_index n = #2;
        char * buf = raw_buffer_pointer(env, #1, #2);

	MKCL_LIBC_NO_INTR(env, fill_inet_sockaddr(&sockaddr, #3, #4, #5, #6, #7));
        do {
          mkcl_index res;

          MKCL_LIBC_Zzz(env, #8, (res = sendto(#0, buf + len, n, flags, (struct sockaddr*)&sockaddr, sizeof(struct sockaddr_in))));
          if ((res == -1) && (errno != EINTR)) {
            len = -1;
            break; /* stop writing */
          } else {			
            len += res;
            n -= res;
          }
          mk_mt_test_for_thread_shutdown(env);
        } while (n > 0);

        @(return) = len;
}
"
		     :one-liner nil))
	       (c-inline (fd buffer length :io
			     oob eor dontroute dontwait nosignal
			     #+linux confirm #-linux nil)
		     (:int :object :int :object
			   :bool :bool :bool :bool :bool :bool)
		     :long
		     "
{
        int flags = ( #4 ? MSG_OOB : 0 )  |
                    ( #5 ? MSG_EOR : 0 ) |
                    ( #6 ? MSG_DONTROUTE : 0 ) |
                    ( #7 ? MSG_DONTWAIT : 0 ) |
                    ( #8 ? MSG_NOSIGNAL : 0 )
#if __linux
                    | ( #9 ? MSG_CONFIRM : 0 )
#endif
                    ;
        mkcl_type type = mkcl_type_of(#1);
        ssize_t len = 0;
        mkcl_index n = #2;
        char * buf = raw_buffer_pointer(env, #1, #2);

        do {
          mkcl_index res;

          MKCL_LIBC_Zzz(env, #3, (res = send(#0, buf + len, n, flags)));
          if ((res == -1) && (errno != EINTR)) {
            len = -1;
            break; /* stop writing */
          } else {			
            len += res;
            n -= res;
          }
          mk_mt_test_for_thread_shutdown(env);
        } while (n > 0);

        @(return) = len;
}
"
		     :one-liner nil))))
      (if (= len-sent -1)
	  (socket-error "send")
	  len-sent))))

#+:windows
(defmethod socket-close-low-level ((socket inet-socket))
  (ff-closesocket (socket-file-descriptor socket)))

#+:windows
(defmethod socket-send ((socket socket) buffer length
			   &key address external-format dontroute oob eor dontwait nosignal confirm more)
  (declare (ignore external-format eor dontwait nosignal confirm more))
  (assert (or (stringp buffer)
		(typep buffer 'vector)))
  (let (;eh, here goes string->octet convertion... 
	;When will ecl support Unicode?
	(length (or length (length buffer)))
	(fd (socket-file-descriptor socket)))
    (let ((len-sent
	   (if address
	       (progn
		 (assert (= 2 (length address)))
		 (c-inline (fd buffer length 
			       (second address)
			       (aref (first address) 0)
			       (aref (first address) 1)
			       (aref (first address) 2)
			       (aref (first address) 3)
			       dontroute oob :io)
		     (:int :object :int
			   :int :int :int :int :int
			   :bool :bool :object)
		     :long
		     "
{
        int flags = ( #8 ? MSG_DONTROUTE : 0 ) | ( #9 ? MSG_OOB : 0 );
        mkcl_type type = mkcl_type_of(#1);
        struct sockaddr_in sockaddr;
        const int sockaddr_len = sizeof(sockaddr);
	ssize_t len;

	MKCL_LIBC_NO_INTR(env, fill_inet_sockaddr(&sockaddr, #3, #4, #5, #6, #7));

{
      SOCKET s = (SOCKET) #0;
      int rc;
      BOOL ok; 
      WSABUF DataBuf = { #2, raw_buffer_pointer(env, #1, #2) };
      DWORD BytesSent = 0;
      DWORD Flags = 0;
      WSAOVERLAPPED SendOverlapped = { 0 };

      MKCL_LIBC_NO_INTR(env, rc = WSASendTo(s, &DataBuf, 1, &BytesSent, Flags, (SOCKADDR *) &sockaddr, sockaddr_len,  &SendOverlapped, _socket_send_io_done));

      if (rc == 0)
	{
	  DWORD wait_val;

	  MKCL_LIBC_Zzz(env, #a, wait_val = SleepEx(0, TRUE));

	  if (wait_val != WAIT_IO_COMPLETION)
	    mkcl_FEwin32_error(env, \"WSASendTo() (sockets.lisp) failed to complete properly on socket ~S\", 0);

	  mk_mt_test_for_thread_shutdown(env);

          len = BytesSent;
        }
      else if (rc == SOCKET_ERROR)
	{
	  DWORD wait_val;

	  if (WSAGetLastError() != WSA_IO_PENDING)
            { @(return) = -1; goto _MKCL_SENDTO_ERROR; }

	  do {
	    MKCL_LIBC_Zzz(env, #a, wait_val = SleepEx(INFINITE, TRUE));
	  } while ((wait_val == WAIT_IO_COMPLETION)
		   && (WSAGetOverlappedResult(s, &SendOverlapped, &BytesSent, FALSE, &Flags)
		       ? FALSE
		       : ((WSAGetLastError() == WSA_IO_INCOMPLETE)
			  ? TRUE
			  : (mkcl_FEwin32_error(env, \"WSAGetOverlappedResult() (sockets.lisp) failed unexpectedtly after WSASendTo() on socket\", 0), FALSE))));
	  if (wait_val != WAIT_IO_COMPLETION)
	    mkcl_FEwin32_error(env, \"WSASendTo() (sockets.lisp) failed to properly complete deferred IO on socket ~S\", 0);

	  mk_mt_test_for_thread_shutdown(env);

          len = BytesSent;
	}
      else
        { @(return) = -1; goto _MKCL_SENDTO_ERROR; }
}

        @(return) = len;
_MKCL_SENDTO_ERROR:;
}
"
		     :one-liner nil))
	       (c-inline (fd buffer length dontroute oob :io)
		     (:int :object :int :bool :bool :object)
		     :long
		     "
{
        int flags = ( #3 ? MSG_DONTROUTE : 0 ) |( #4 ? MSG_OOB : 0 );
        mkcl_type type = mkcl_type_of(#1);
        ssize_t len;

      SOCKET s = (SOCKET) #0;
      int rc;
      BOOL ok; 
      WSABUF DataBuf = { #2, raw_buffer_pointer(env, #1,#2) };
      DWORD BytesSent = 0;
      DWORD Flags = flags;
      WSAOVERLAPPED SendOverlapped = { 0 };

      MKCL_LIBC_NO_INTR(env, rc = WSASend(s, &DataBuf, 1, &BytesSent, Flags, &SendOverlapped, _socket_send_io_done));

      if (rc == 0)
	{
	  DWORD wait_val;

	  MKCL_LIBC_Zzz(env, #5, wait_val = SleepEx(0, TRUE));

	  if (wait_val != WAIT_IO_COMPLETION)
	    mkcl_FEwin32_error(env, \"WSASend() (sockets.lisp) failed to complete properly on socket ~S\", 0);

	  mk_mt_test_for_thread_shutdown(env);

          len = BytesSent;
        }
      else if (rc == SOCKET_ERROR)
	{
	  DWORD wait_val;

	  if (WSAGetLastError() != WSA_IO_PENDING)
            { @(return) = -1; goto _MKCL_SEND_ERROR; }

	  do {
	    MKCL_LIBC_Zzz(env, #5, wait_val = SleepEx(INFINITE, TRUE));
	  } while ((wait_val == WAIT_IO_COMPLETION)
		   && (WSAGetOverlappedResult(s, &SendOverlapped, &BytesSent, FALSE, &Flags)
		       ? FALSE
		       : ((WSAGetLastError() == WSA_IO_INCOMPLETE)
			  ? TRUE
			  : (mkcl_FEwin32_error(env, \"WSAGetOverlappedResult() (sockets.lisp) failed unexpectedtly after WSASend() on socket\", 0), FALSE))));
	  if (wait_val != WAIT_IO_COMPLETION)
	    mkcl_FEwin32_error(env, \"WSASend() (sockets.lisp) failed to properly complete deferred IO on socket ~S\", 0);

	  mk_mt_test_for_thread_shutdown(env);

          len = BytesSent;
	}
      else
        { @(return) = -1; goto _MKCL_SEND_ERROR; }

        @(return) = len;

_MKCL_SEND_ERROR:;
}
"
		     :one-liner nil))))
      (if (= len-sent -1)
	  (socket-error "send")
	  len-sent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNIX SOCKETS
;;;

#-:windows
(progn

(defclass local-socket (socket)
  ((family :initform +af-local+))
  (:documentation "Class representing local domain (AF_LOCAL) sockets,
also known as unix-domain sockets."))


(defmethod socket-bind ((socket local-socket) &rest address)
  (assert (= 1 (length address)) (address) "Socket-bind needs two parameters for local sockets.")
  (let ((name (first address))
	(fd (socket-file-descriptor socket))
	(family (socket-family socket)))
    (if (= -1
	   (c-inline (fd name family) (:int :cstring :int) :int
		     "
{
        struct sockaddr_un sockaddr;
	size_t size;
	int output;
#ifdef BSD
        sockaddr.sun_len = sizeof(struct sockaddr_un);
#endif
        sockaddr.sun_family = #2;
        strncpy(sockaddr.sun_path,#1,sizeof(sockaddr.sun_path));
	sockaddr.sun_path[sizeof(sockaddr.sun_path)-1] = '\\0';

	MKCL_LIBC_NO_INTR(env, (output = bind(#0,(struct sockaddr*)&sockaddr, sizeof(struct sockaddr_un))));

        @(return) = output;
}"))
	(socket-error "bind"))))

(defmethod socket-accept ((socket local-socket))
  (multiple-value-bind (fd name)
      (c-inline ((socket-file-descriptor socket) :io) (:int :object) (values :int :object)
"{
        struct sockaddr_un sockaddr;
        socklen_t addr_len = (socklen_t)sizeof(struct sockaddr_un);
        int new_fd;

	MKCL_LIBC_Zzz(env, #1, (new_fd = accept(#0, (struct sockaddr *)&sockaddr, &addr_len)));
        mk_mt_test_for_thread_shutdown(env);

	@(return 0) = new_fd;
	@(return 1) = (new_fd == -1) ? mk_cl_Cnil : mkcl_make_base_string_copy(env, sockaddr.sun_path);
}") ;; FIXME: On Windows this code should be using AcceptEx with an alertable wait. JCB
    (cond
      ((= fd -1)
       (socket-error "accept on local socket"))
      (t
       (values
	(make-instance (class-of socket)
		       :type (socket-type socket)
		       :protocol (socket-protocol socket)
		       :descriptor fd)
	name)))))

(defmethod socket-connect ((socket local-socket) &rest address)
  (assert (= 1 (length address)) (address) "Socket-connect needs two parameters for local sockets.")
  (let ((path (first address))
	(fd (socket-file-descriptor socket))
	(family (socket-family socket)))
    (if (= -1
	   (c-inline (fd family path :io) (:int :int :cstring :object) :int
		     "
{
        struct sockaddr_un sockaddr;
	int output;
#ifdef BSD
        sockaddr.sun_len = sizeof(struct sockaddr_un);
#endif
        sockaddr.sun_family = #1;
        strncpy(sockaddr.sun_path,#2,sizeof(sockaddr.sun_path));
	sockaddr.sun_path[sizeof(sockaddr.sun_path)-1] = '\\0';

	MKCL_LIBC_Zzz(env, #3, (output = connect(#0, (struct sockaddr*)&sockaddr, sizeof(struct sockaddr_un))));
        mk_mt_test_for_thread_shutdown(env);

        @(return) = output;
}")) ;; FIXME: On Windows this code should use connectEx with an alertable wait. JCB
	(socket-error "connect"))))

(defmethod socket-peername ((socket local-socket))
  (let* ((fd (socket-file-descriptor socket))
	 (peer (c-inline (fd) (:int) t
			 "
{
        struct sockaddr_un name;
        socklen_t len = sizeof(struct sockaddr_un);
        int ret;

	MKCL_LIBC_NO_INTR(env, (ret = getpeername(#0,(struct sockaddr*)&name,&len)));

        if (ret == 0) {
                @(return) = mkcl_make_base_string_copy(env, name.sun_path);
        } else {
                @(return) = mk_cl_Cnil;
        }
}")))
    (if peer
	peer
	(socket-error "getpeername"))))

) ;#-:windows


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNIX SOCKETS [WIN32, using the cygwin way]
;;;

#+:windows
(progn

(defclass local-socket (socket)
  ((family :initform +af-local+)
   proxy-socket local-path)
  (:documentation "Class representing local domain (AF_LOCAL) sockets,
also known as unix-domain sockets."))

(defmethod initialize-instance :after ((socket local-socket) &rest args)
  (declare (ignore args))
  (with-slots (protocol type) socket
    (setf (slot-value socket 'proxy-socket)
          (make-instance 'inet-socket :protocol protocol :type type))))

(defmethod socket-bind ((socket local-socket) &rest address)
  (assert (= 1 (length address)) (address) "Socket-bind needs two parameters for local sockets.")
  (with-slots (proxy-socket local-path) socket
    (socket-bind proxy-socket #(127 0 0 1) 0)
    (multiple-value-bind (ip port) (socket-peername proxy-socket)
	(declare (ignore ip))
      (handler-case 
        (with-open-file (fd (first address) :if-exists :error :if-does-not-exist :create :direction :output)
	  (format fd "!<socket >~D 00000000-00000000-00000000-00000000" port))
	(file-error ()
	  (socket-close proxy-socket)
	  (c-inline () () nil "WSASetLastError(WSAEADDRINUSE)" :one-liner t)
	  (socket-error "socket-bind")))
      (setf local-path (first address))
      socket)))

(defmethod socket-accept ((socket local-socket))
  (multiple-value-bind (new-socket addr) (socket-accept (slot-value socket 'proxy-socket))
       (declare (ignore new-socket addr))
    (values socket (slot-value socket 'local-path))))

(defmethod socket-connect ((socket local-socket) &rest address)
  (assert (= 1 (length address)) (address) "Socket-connect needs two parameters for local sockets.")
  (with-slots (proxy-socket local-path) socket
    (handler-case
      (with-open-file (fd (first address) :if-does-not-exist :error :direction :input)
        (let ((buf (make-string 128)) port)
	  (read-sequence buf fd)
	  (unless (and (string-equal "!<socket >" (subseq buf 0 10))
	               (typep (setq port (read-from-string (subseq buf 10) nil 'eof)) '(integer 0 65535)))
	    (c-inline () () nil "WSASetLastError(WSAEFAULT)" :one-liner t)
	    (socket-error "connect"))
	  (prog1
	    (socket-connect proxy-socket #(127 0 0 1) port)
	    (setf local-path (first address)))))
      (file-error ()
        (socket-error "connect")))))

(defmethod socket-peername ((socket local-socket))
  (unless (slot-boundp socket 'local-path)
    (c-inline () () nil "WSASetLastError(WSAENOTCONN)" :one-liner t)
    (socket-error "socket-peername"))
  (slot-value socket 'local-path))

(defmethod socket-close ((socket local-socket))
  (socket-close (slot-value socket 'proxy-socket))
  (slot-makunbound socket 'local-path))

(defmethod socket-make-stream ((socket local-socket) &rest args)
  (apply #'socket-make-stream (cons (slot-value socket 'proxy-socket) args)))

(defmethod non-blocking-mode ((socket local-socket))
  (non-blocking-mode (slot-value socket 'proxy-socket)))

(defmethod (setf non-blocking-mode) (non-blocking-p (socket local-socket))
  (setf (non-blocking-mode (slot-value socket 'proxy-socket)) non-blocking-p))

) ;#+:windows

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NAMED PIPE SOCKETS [WIN32]
;;;

#+:windows
(progn

(defclass named-pipe-socket (socket)
  ((family :initform +af-named-pipe+)
   (pipe-name :initarg :pipe-name))
  (:documentation "Class representing Win32 named pipe, using a socket-like interface."))

(defmethod socket-bind ((socket named-pipe-socket) &rest address)
  (assert (= 1 (length address)) (address) "Socket-bind needs two parameters for local sockets.")
  (let* ((pipe-name (concatenate 'string "\\\\.\\pipe\\" (first address)))
         (hnd (c-inline (pipe-name) (:cstring) :int
	                "
{
	HANDLE hnd;
	MKCL_LIBC_NO_INTR(env, (hnd = CreateNamedPipe(#0,
			                         PIPE_ACCESS_DUPLEX,
			                         PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
			                         PIPE_UNLIMITED_INSTANCES,
			                         4096,
			                         4096,
			                         NMPWAIT_USE_DEFAULT_WAIT,
			                         NULL)));
	if (hnd == INVALID_HANDLE_VALUE)
		@(return) = -1;
	else
		@(return) = _open_osfhandle((intptr_t)hnd, O_RDWR);
}")))
    (when (= hnd -1)
      (socket-error "CreateNamedPipe"))
    (setf (slot-value socket 'pipe-name) pipe-name)
    (setf (slot-value socket 'file-descriptor) hnd)))

(defmethod socket-accept ((socket named-pipe-socket))
  (let* ((fd (socket-file-descriptor socket))
         (afd (c-inline (fd) (:int) :int
	                "
{
	HANDLE hnd = (HANDLE) _get_osfhandle(#0), dupHnd;

        MKCL_LIBC_NO_INTR(env, (@(return) = ((ConnectNamedPipe(hnd, NULL) != 0 || GetLastError() == ERROR_PIPE_CONNECTED) ? #0 : -1)));
}"
                        :one-liner nil))) ;; FIXME: This code should use overlapped IO with an alertable wait. JCB
    (cond
      ((= afd -1)
       (socket-error "accept on named pipe"))
      (t
       ;; rebind the socket to create a new named pipe instance in the server
       (socket-bind socket (subseq (slot-value socket 'pipe-name) 9))
       (values
	(make-instance (class-of socket)
		       :type (socket-type socket)
		       :protocol (socket-protocol socket)
		       :descriptor afd
		       :pipe-name (slot-value socket 'pipe-name))
	(slot-value socket 'pipe-name))))))

(defmethod socket-connect ((socket named-pipe-socket) &rest address)
  (assert (= 1 (length address)) (address) "Socket-connect needs two parameters for local sockets.")
  (let* ((path (first address))
	 (pipe-name (concatenate 'string "\\\\.\\pipe\\" path)))
    (if (= -1
	   (setf (slot-value socket 'file-descriptor)
	         (c-inline (pipe-name) (:cstring) :int
		     "
{
	HANDLE hnd;

        MKCL_LIBC_NO_INTR(env, (hnd = CreateFile(#0,GENERIC_READ | GENERIC_WRITE,0,NULL,OPEN_EXISTING,0,NULL)));
        MKCL_LIBC_NO_INTR(env, (@(return) = ((hnd == INVALID_HANDLE_VALUE) ? -1 : _open_osfhandle((intptr_t)hnd, O_RDWR))));
}")))
	(socket-error "connect")
	(setf (slot-value socket 'pipe-name) pipe-name))))

(defmethod socket-peername ((socket named-pipe-socket))
  (slot-value socket 'pipe-name))

(defmethod (setf non-blocking-mode) (non-blocking-p (socket named-pipe-socket))
  (let ((fd (socket-file-descriptor socket)))
    (if (= 0
           (c-inline (fd non-blocking-p) (:int t) :int
                     "
{
	DWORD mode = PIPE_READMODE_BYTE | (#1 == mk_cl_Ct ? PIPE_NOWAIT : PIPE_WAIT);
	MKCL_LIBC_NO_INTR(env, (@(return) = SetNamedPipeHandleState((HANDLE) _get_osfhandle(#0), &mode, NULL, NULL)));
}"
                     :one-liner nil))
      (socket-error "SetNamedPipeHandleState")
      (setf (slot-value socket 'non-blocking-p) non-blocking-p))))

(defmethod socket-close ((socket named-pipe-socket))
  (let ((fd (socket-file-descriptor socket)))
    (unless (c-inline (fd) (:int) t
                  "
{
	DWORD flags;
        BOOL ok;

        MKCL_LIBC_NO_INTR(env, (ok = GetNamedPipeInfo((HANDLE) _get_osfhandle(#0), &flags, NULL, NULL, NULL)));
        if (!ok)
          @(return) = mk_cl_Cnil;
        else if (flags == PIPE_CLIENT_END)
          @(return) = mk_cl_Ct;
        else
          {
            MKCL_LIBC_NO_INTR(env, (ok = DisconnectNamedPipe((HANDLE) _get_osfhandle(#0))));
            if (ok)
              @(return) = mk_cl_Ct;
            else
              @(return) = mk_cl_Cnil;
          }
}"
                  :one-liner nil)
      (socket-error "DisconnectNamedPipe"))
    (call-next-method)))

) ;#+:windows

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NON-BLOCKING MODE
;;;

(defmethod non-blocking-mode ((socket socket))
  #-:windows
  (let ((fd (socket-file-descriptor socket)))
    (not (zerop (c-inline (fd) (:int) :int "fcntl(#0,F_GETFL,NULL)&O_NONBLOCK" :one-liner t))))
  #+:windows
  (slot-value socket 'non-blocking-p)
)

(defmethod (setf non-blocking-mode) (non-blocking-p (socket socket))
  (let ((fd (socket-file-descriptor socket))
	(nblock (if non-blocking-p 1 0)))
    (if (= -1 (c-inline (fd nblock) (:int :int) :int
	      #+:windows
	      "
{
	int blocking_flag = (#1 ? 1 : 0);
	MKCL_LIBC_NO_INTR(env, (@(return) = ioctlsocket(#0, FIONBIO, (u_long*)&blocking_flag)));
}"
	      #-:windows
	      "
{
        int oldflags;
        int newflags;

        MKCL_LIBC_NO_INTR(env, (oldflags = fcntl(#0,F_GETFL,NULL)));
        newflags = (oldflags & ~O_NONBLOCK) | (#1 ? O_NONBLOCK : 0);
        MKCL_LIBC_NO_INTR(env, (@(return) = fcntl(#0,F_SETFL,newflags)));
}"))
	(socket-error #-:windows "fcntl" #+:windows "ioctlsocket")
	#-:windows non-blocking-p
	#+:windows (setf (slot-value socket 'non-blocking-p) non-blocking-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STREAMS
;;;
;;; To actually read/write to/from the sockets, we use Lisp streams. The
;;; following functions take care of building the streams. Fortunately
;;; we do not have to care about things like buffering, binary streams,
;;; etc, but we rather reuse the code from the C core. (For instance
;;; the sockets will be closed upon garbage collection)
;;;

(defun make-stream-from-fd (fd mode &key (element-type 'character)
					 (external-format :default)
					 (name "SOCKET-FD-STREAM"))
  (assert (stringp name) (name) "name must be a string.")
  (let* ((smm-mode (ecase mode
			 (:input (c-constant "mkcl_smm_input_file"))
			 (:output (c-constant "mkcl_smm_output_file"))
			 (:io (c-constant "mkcl_smm_io_file"))
			 (:input-socket (c-constant "mkcl_smm_input_socket"))
			 (:output-socket (c-constant "mkcl_smm_output_socket"))
			 (:io-socket (c-constant "mkcl_smm_io_socket"))
			 )))
      (c-inline (name fd smm-mode element-type external-format) (t :int :int t t) t
		"mkcl_make_stream_from_fd(env, #0, #1, (enum mkcl_smmode)#2, #3, #4)"
		:one-liner t)))


(defmethod socket-make-stream ((socket socket)  &rest args
			       &key (element-type 'character) (buffering :full) (external-format :default))
  (declare (ignore args buffering))
  ;; Trying to fit a buffering scheme designed for block devices onto a network device is utter nonsense.
  ;; We simply ignore the whole thing.  If you really need control over socket "buffering" use
  ;; the standard socket option SO_SNDBUF and/or SO_RCVBUF below.
  (let ((stream (and (slot-boundp socket 'stream)
		     (slot-value socket 'stream))))
    (unless stream
      (setf stream (let ((fd (socket-file-descriptor socket)))
		     (make-stream-from-fd fd :io-socket
					  :element-type element-type
					  :external-format external-format
					  :name "SOCKET-IO-STREAM")))
      (setf (slot-value socket 'stream) stream)
      )
    stream))

#+:windows
(defmethod socket-make-stream ((socket named-pipe-socket) &rest args
			       &key (element-type 'character) (buffering NIL) (external-format :default))
  (declare (ignore args buffering)) ;; buffering on a message based device! No kidding! JCB
  (let ((stream (and (slot-boundp socket 'stream)
		     (slot-value socket 'stream))))
    (unless stream
      (setf stream
	    (let* ((fd (socket-file-descriptor socket))
		   (in (make-stream-from-fd fd :input :element-type element-type
					    :external-format external-format :name "NAMED-PIPE-IN-STREAM"))
		   (out (make-stream-from-fd fd :output :element-type element-type
					     :external-format external-format :name "NAMED-PIPE-OUT-STREAM")))
	      (make-two-way-stream in out))) ;; Why the two-way stuff here? Named pipes are bidir anyway. JCB
      (setf (slot-value socket 'stream) stream)
      #+ ignore
      (sb-ext:cancel-finalization socket))
    stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ERROR HANDLING
;;;
;;; A plethora of conditions are defined below, almost one for each
;;; possible error produced by the socket or DNS interface.
;;;

#+:windows
(defun get-win32-error-string (num)
  (c-inline (num) (:int) t
	"{
          DWORD msg_size;
	  mkcl_object msg;
          wchar_t * lpMsgBuf;
	  MKCL_LIBC_NO_INTR(env, (msg_size = FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
	                                               NULL,
	                                               #0,
	                                               0,
	                                               (void *)&lpMsgBuf,
	                                               0,
	                                               NULL)));
          if (msg_size == 0)
            msg = mkcl_make_simple_base_string(env, \"[Unable to get OS error message]\");
          else {
	    msg = mkcl_cstring16_to_string(env, lpMsgBuf);
	    MKCL_LIBC_NO_INTR(env, LocalFree(lpMsgBuf));
          }
	  @(return) = msg;}"
	  :one-liner nil))

;;;
;;; 1) SOCKET ERRORS
;;;

(define-condition socket-error (error)
  ((errno :initform nil
          :initarg :errno
          :reader socket-error-errno)
   (symbol :initform nil :initarg :symbol :reader socket-error-symbol)
   (syscall  :initform "outer space" :initarg :syscall :reader socket-error-syscall))
  (:report (lambda (c s)
             (let ((num (socket-error-errno c)))
               (format s "Socket error in \"~A\": ~A (~A)"
                       (socket-error-syscall c)
                       (or (socket-error-symbol c) (socket-error-errno c))
		       #+:windows
		       (get-win32-error-string num)
		       #-:windows
		       (si:libc-error-string num)))))
  (:documentation "Common base class of socket related conditions."))

(defmacro define-socket-condition (symbol name)
  `(progn
     (defparameter ,symbol (c-constant ,(symbol-name symbol)))
     (define-condition ,name (socket-error)
       ((symbol :reader socket-error-symbol :initform (quote ,symbol))))
     (export ',name)
     (push (cons ,symbol (quote ,name)) *conditions-for-errno*)))

(defparameter *conditions-for-errno* nil)
;;; this needs the rest of the list adding to it, really.  They also
;;; need symbols to be added to constants.ccon
;;; I haven't yet thought of a non-kludgey way of keeping all this in
;;; the same place
#+:windows
(Clines
 "#ifdef __MINGW64_VERSION_MAJOR"
 "#undef EADDRINUSE"
 "#undef ECONNREFUSED"
 "#undef ETIMEDOUT"
 "#undef ENOBUFS"
 "#undef EOPNOTSUPP"
 "#undef EPROTONOSUPPORT"
 "#undef ESOCKTNOSUPPORT"
 "#undef ENETUNREACH"
 "#endif"
 "#define EADDRINUSE WSAEADDRINUSE"
 "#define ECONNREFUSED WSAECONNREFUSED"
 "#define ETIMEDOUT WSAETIMEDOUT"
 "#define ENOBUFS WSAENOBUFS"
 "#define EOPNOTSUPP WSAEOPNOTSUPP"
 "#define EPROTONOSUPPORT WSAEPROTONOSUPPORT"
 "#define ESOCKTNOSUPPORT WSAESOCKTNOSUPPORT"
 "#define ENETUNREACH WSAENETUNREACH"
 "#define NETDB_INTERNAL WSAEAFNOSUPPORT"
 "#define NETDB_SUCCESS 0"
 )
(define-socket-condition EADDRINUSE address-in-use-error)
(define-socket-condition EAGAIN unavailable-error)
(define-socket-condition EBADF bad-file-descriptor-error)
(define-socket-condition ECONNREFUSED connection-refused-error)
(define-socket-condition ETIMEDOUT operation-timeout-error)
(define-socket-condition EINTR interrupted-error)
(define-socket-condition EINVAL invalid-argument-error)
(define-socket-condition ENOBUFS no-buffers-error)
(define-socket-condition ENOMEM out-of-memory-error)
(define-socket-condition EOPNOTSUPP operation-not-supported-error)
(define-socket-condition EPERM operation-not-permitted-error)
(define-socket-condition EPROTONOSUPPORT protocol-not-supported-error)
(define-socket-condition ESOCKTNOSUPPORT socket-type-not-supported-error)
(define-socket-condition ENETUNREACH network-unreachable-error)


(defun condition-for-errno (err)
  (or (cdr (assoc err *conditions-for-errno* :test #'eql)) 'socket-error))

(defun socket-error (where)
  (let* ((errno  (c-constant #-:windows "errno" #+:windows "WSAGetLastError()"))
         (condition (condition-for-errno errno)))
    (error condition :errno errno  :syscall where)))

;;;
;;; 2) DNS ERRORS
;;;

(defvar *name-service-errno* 0
  "The value of h_errno, after it's been fetched from Unix-land by calling
GET-NAME-SERVICE-ERRNO")

(defun name-service-error (where)
  (get-name-service-errno)
  ;; Comment next to NETDB_INTERNAL in netdb.h says "See errno.".
  ;; This special case treatment hasn't actually been tested yet.
  (if (= *name-service-errno* (c-constant "NETDB_INTERNAL"))
      (socket-error where)
    (let ((condition
	   (condition-for-name-service-errno *name-service-errno*)))
      (error condition :errno *name-service-errno* :syscall where))))

(define-condition name-service-error (error) ;;(condition) ;; JCB 
  ((errno :initform nil
	  :initarg :errno
	  :reader name-service-error-errno)
   (symbol :initform nil :initarg :symbol :reader name-service-error-symbol)
   (syscall :initform "an unknown location" :initarg :syscall :reader name-service-error-syscall))
  (:report (lambda (c s)
	     (let ((num (name-service-error-errno c)))
	       (format s "Name service error in \"~A\": ~A (~A)"
		       (name-service-error-syscall c)
		       (or (name-service-error-symbol c)
			   (name-service-error-errno c))
		       (get-name-service-error-message num))))))

(defmacro define-name-service-condition (symbol name)
  `(progn
     (defparameter ,symbol (c-constant ,(symbol-name symbol)))
     (define-condition ,name (name-service-error)
       ((symbol :reader name-service-error-symbol :initform (quote ,symbol))))
     (push (cons ,symbol (quote ,name)) *conditions-for-name-service-errno*)
     (export (quote ,symbol))))

(defparameter *conditions-for-name-service-errno* nil)

(define-name-service-condition NETDB_INTERNAL netdb-internal-error)
(define-name-service-condition NETDB_SUCCESS netdb-success-error)
(define-name-service-condition HOST_NOT_FOUND host-not-found-error)
(define-name-service-condition TRY_AGAIN try-again-error)
(define-name-service-condition NO_RECOVERY no-recovery-error)
;; this is the same as the next one
;;(define-name-service-condition NO_DATA no-data-error)
(define-name-service-condition NO_ADDRESS no-address-error)

(defun condition-for-name-service-errno (err)
  (or (cdr (assoc err *conditions-for-name-service-errno* :test #'eql))
      'name-service))

(defun get-name-service-errno ()
  (setf *name-service-errno* (c-constant #-:windows "h_errno" #+:windows "WSAGetLastError()")))

(defun get-name-service-error-message (num)
  #+:windows
  (get-win32-error-string num)
  #-:windows
  (si:libc-error-string num)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SOCKET OPTIONS
;;;

(defun get-sockopt-int (fd const)
  (let ((ret (c-inline (fd const) (:int :int) t
"{
        int sockopt, ret;
        socklen_t socklen = sizeof(sockopt);

	MKCL_LIBC_NO_INTR(env, (ret = getsockopt(#0,SOL_SOCKET,#1, (void *) &sockopt,&socklen)));

        @(return) = (ret == 0) ? mkcl_make_integer(env, sockopt) : mk_cl_Cnil;
}")))
    (if ret
	ret
	(error "Sockopt error: ~A" (si:errno-string)))))


(defun get-sockopt-bool (fd const)
  (let ((ret (c-inline (fd const) (:int :int) t
"{
        int sockopt, ret;
        socklen_t socklen = sizeof(sockopt);

        MKCL_LIBC_NO_INTR(env, (ret = getsockopt(#0,SOL_SOCKET,#1, (void *) &sockopt,&socklen)));

        @(return) = (ret == 0) ? mkcl_make_integer(env, sockopt) : mk_cl_Cnil;
}")))
    (if ret
	(/= ret 0)
	(error "Sockopt error: ~A" (si:errno-string)))))

#+windows
(defun get-sockopt-timeval (fd const)
  (* 1000 (get-sockopt-int fd const)))

#-windows
(defun get-sockopt-timeval (fd const)
  (let ((ret (c-inline (fd const) (:int :int) t
"{
	struct timeval tv;
        socklen_t socklen = sizeof(tv);
        int ret;

	MKCL_LIBC_NO_INTR(env, (ret = getsockopt(#0,SOL_SOCKET,#1,&tv,&socklen)));

        @(return) = (ret == 0) ? mkcl_make_doublefloat(env, (double)tv.tv_sec
					+ ((double)tv.tv_usec) / 1000000.0) : mk_cl_Cnil;
}")))
    (if ret
	ret
	(error "Sockopt error: ~A" (si:errno-string)))))

(defun set-sockopt-int (fd const value)
  (let ((ret (c-inline (fd const value) (:int :int :int) t
"{
        int sockopt = #2;
        int ret;

	MKCL_LIBC_NO_INTR(env, (ret = setsockopt(#0,SOL_SOCKET,#1,(void *) &sockopt,sizeof(sockopt))));

        @(return) = (ret == 0) ? mk_cl_Ct : mk_cl_Cnil;
}")))
    (if ret
	value
	(error "Sockopt error: ~A" (si:errno-string)))))

(defun set-sockopt-bool (fd const value)
  (let ((ret (c-inline (fd const value) (:int :int :object) t
"{
        int sockopt = (#2 == mk_cl_Cnil) ? 0 : 1;
        int ret;

	MKCL_LIBC_NO_INTR(env, (ret = setsockopt(#0,SOL_SOCKET,#1, (void *) &sockopt,sizeof(sockopt))));

        @(return) = (ret == 0) ? mk_cl_Ct : mk_cl_Cnil;
}")))
    (if ret
	value
	(error "Sockopt error: ~A" (si:errno-string)))))

#-windows
(defun set-sockopt-timeval (fd const value)
  (let ((ret (c-inline (fd const value) (:int :int :double) t
"{
	struct timeval tv;
	double tmp = #2;
	int ret;

	tv.tv_sec = (long)tmp;
	tv.tv_usec = (long)((tmp-floor(tmp))*1000000.0);
        MKCL_LIBC_NO_INTR(env, (ret = setsockopt(#0, SOL_SOCKET, #1, &tv, sizeof(tv))));

        @(return) = (ret == 0) ? mk_cl_Ct : mk_cl_Cnil;
}")))
    (if ret
	value
	(error "Sockopt error: ~A" (si:errno-string)))))

#+windows
(defun set-sockopt-timeval (fd const value)
  (set-sockopt-int fd const (* 1000 value)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-sockopt (name c-const type &optional (read-only nil))
    `(progn
       (export ',name)
       (defun ,name (socket)
	 (,(intern (format nil "GET-SOCKOPT-~A" type))
	   (socket-file-descriptor socket)
	   (c-constant ,c-const)))
       ,@(unless read-only
	   `((defun (setf ,name) (value socket)
	       (,(intern (format nil "SET-SOCKOPT-~A" type))
		 (socket-file-descriptor socket)
		 (c-constant ,c-const)
		 value)))))))

(define-sockopt sockopt-type "SO_TYPE" int t)
(define-sockopt sockopt-receive-buffer "SO_RCVBUF" int)
(define-sockopt sockopt-send-buffer "SO_SNDBUF" int)
(define-sockopt sockopt-receive-timeout "SO_RCVTIMEO" timeval)
(define-sockopt sockopt-send-timeout "SO_SNDTIMEO" timeval)
(define-sockopt sockopt-reuse-address "SO_REUSEADDR" bool)
(define-sockopt sockopt-keep-alive "SO_KEEPALIVE" bool)
(define-sockopt socket-dont-route "SO_DONTROUTE" bool)
(define-sockopt socket-linger "SO_LINGER" bool)

#-(or :unix :linux :windows :cygwin)
(define-sockopt sockopt-reuse-port "SO_REUSEPORT" bool)

(define-sockopt sockopt-tcp-nodelay "TCP_NODELAY" bool)

;; Add sockopts here as you need them...

;; Finished loading
(provide 'sockets)

;; (terpri)
;; (princ ";;; Loading sockets.lisp DONE!")
;; (finish-output)
