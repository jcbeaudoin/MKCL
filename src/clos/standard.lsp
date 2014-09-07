;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2014, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "CLOS")


(defmethod finalize-inheritance ((class forward-referenced-class))
  (error "Unable to finalize class ~A. Its class definition is missing." (class-name class)))


;;; ----------------------------------------------------------------------
;;; CHECK INITARGS
;;;
;;; There are different sets of initialization arguments. First we have
;;; those coming from the :INITARG option in the slots. Then we have
;;; all declared initargs which are keyword arguments to methods defined
;;; on SHARED-INITIALIZE, REINITIALIZE-INSTANCE, etc. (See ANSI 7.1.2)
;;;

(defun valid-keywords-from-methods (methods)
  ;; Given a list of methods, build up the list of valid keyword arguments
  (do ((m methods (rest m))
       (keys '()))
      ((null m)
       (values keys nil))
    (multiple-value-bind (reqs nb_reqs opts nb_opts rest key-flag keywords nb_keys allow-other-keys)
	(si::process-lambda-list (method-lambda-list (first m)) t)
      (declare (ignore reqs nb_reqs opts nb_opts rest key-flag nb_keys))
      (when allow-other-keys
	(return (values nil t)))
      (do ((k keywords (cddddr k)))
	  ((null k))
	(push (first k) keys)))))

(defun check-initargs (class initargs method-initargs
		       &aux (slots (class-slots class)))
  ;; First get all initargs which have been declared in the given
  ;; methods, then check the list of initargs declared in the slots
  ;; of the class.

  (progn
    (do* ((name-loc initargs (cddr name-loc))
	  (allow-other-keys nil)
	  (allow-other-keys-found nil)
	  (unknown-key nil))
	 ((null name-loc)
	  (when (and (not allow-other-keys) unknown-key)
	    (simple-program-error "Unknown initialization option ~S for class ~A" unknown-key class)))
      (let ((name (first name-loc)))
	(cond ((null (cdr name-loc))
	       (simple-program-error "No value supplied for the init-name ~S." name))
	      ;; This check must be here, because :ALLOW-OTHER-KEYS is a valid
	      ;; slot-initarg.
	      ((and (eql name :ALLOW-OTHER-KEYS)
		    (not allow-other-keys-found))
	       (setf allow-other-keys (second name-loc)
		     allow-other-keys-found t))
	      ;; The initialization argument has been declared in some method
	      ((member name method-initargs :test #'eq)) ;; (... :test #'eq) JCB
	      ;; Check if the arguments is associated with a slot
	      (;;(find name slots :test #'member :key #'slot-definition-initargs)
	       (find name slots :test #'si:memq :key #'slot-definition-initargs) ;; JCB
	       )
	      (t
	       (setf unknown-key name)))))))

;;; ----------------------------------------------------------------------
;;; INSTANCES INITIALIZATION AND REINITIALIZATION
;;;
(defvar +cached-shared-initialize-emfun+ nil)
(defvar +cached-class-slots+ nil)
(defvar +cached-class-size+ nil)
(defvar +cached-class+ nil)
(defvar +instance-to-initialize+ nil)

(defmethod initialize-instance ((instance T) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'shared-initialize instance 'T initargs))

(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (declare (dynamic-extent initargs))
  (let ((shared-initialize-emfun +cached-shared-initialize-emfun+))
    (if shared-initialize-emfun
	(let ((+cached-shared-initialize-emfun+ nil)
	      (si:*dynamic-cons-stack* si:*dynamic-cons-stack*)
	      )
	  (funcall shared-initialize-emfun (si:dyn-list* instance T initargs) nil)
	  )
      (apply #'shared-initialize instance 'T initargs))
    )
  )

(defmethod reinitialize-instance ((instance T) &rest initargs)
  (declare (dynamic-extent initargs))
  (multiple-value-bind (method-initargs allow-other-keys)
    (valid-keywords-from-methods (append (compute-applicable-methods
					  #'reinitialize-instance (list instance))
					 (compute-applicable-methods
					  #'shared-initialize (list instance t))))
    (unless allow-other-keys
      (check-initargs (class-of instance) initargs method-initargs)))
  (apply #'shared-initialize instance '() initargs)
  )

;;; Right now class T and class standard-object are essentially equivalent
;;; but if someone subclasses T directly this will not be true anymore.
;;; You should then uncomment the following method that should provide
;;; a useful default behavior on T.
;;;
(defmethod shared-initialize ((instance T) slot-names &rest initargs)
  (declare (dynamic-extent initargs))

  ;; initialize-instance slots
  (dolist (slotd (class-slots (class-of instance)))
    (let* ((slot-initargs (slot-definition-initargs slotd))
	   (slot-name (slot-definition-name slotd)))
      (or
       ;; Try to initialize the slot from one of the initargs.
       (do ((l initargs) initarg val)
	   ((null l) nil)
	   (setf initarg (pop l))
	   (when (endp l)
	     (simple-program-error "Wrong number of keyword arguments for SHARED-INITIALIZE, ~A"
				   initargs))
	   (unless (symbolp initarg)
	     (simple-program-error "Not a valid initarg: ~A" initarg))
	   (setf val (pop l))
	   (when (member initarg slot-initargs :test #'eq)
	     (setf (slot-value instance slot-name) val) ;; full metadata peek. JCB
	     (return t)))
       ;; Try to initialize the slot from its initform.
       (when (and slot-names
		  (or (eq slot-names 'T)
		      (member slot-name slot-names))
		  (not (slot-boundp instance slot-name)))
	 (let ((initfun (slot-definition-initfunction slotd)))
	   (when initfun
	     (setf (slot-value instance slot-name) (funcall initfun)) ;; full metadata peek. JCB
	     ))))
      ))
  instance)

;; On standard-object we can be more efficient than on T.
(defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
  ;;
  ;; initialize the instance's slots is a two step process
  ;;   1 A slot for which one of the initargs in initargs can set
  ;;      the slot, should be set by that initarg.  If more than
  ;;      one initarg in initargs can set the slot, the leftmost
  ;;      one should set it.
  ;;
  ;;   2 Any slot not set by step 1, may be set from its initform
  ;;      by step 2.  Only those slots specified by the slot-names
  ;;      argument are set.  If slot-names is:
  ;;       T
  ;;            any slot not set in step 1 is set from its
  ;;            initform
  ;;       <list of slot names>
  ;;            any slot in the list, and not set in step 1
  ;;            is set from its initform
  ;;
  ;;       ()
  ;;            no slots are set from initforms
  ;;

  (declare (dynamic-extent initargs))

  ;; initialize-instance slots
  (dolist (slotd (if (eq +instance-to-initialize+ instance)
		     +cached-class-slots+
		   (class-slots (class-of instance))))
    (let* ((slot-initargs (slot-definition-initargs slotd))
	   (slot-name (slot-definition-name slotd)))
      (or
       ;; Try to initialize the slot from one of the initargs.
       (do ((l initargs) initarg val)
	   ((null l) nil)
	   (setf initarg (pop l))
	   (when (endp l)
	     (simple-program-error "Wrong number of keyword arguments for SHARED-INITIALIZE, ~A" initargs))
	   (unless (symbolp initarg)
	     (simple-program-error "Not a valid initarg: ~A" initarg))
	   (setf val (pop l))
	   (when (member initarg slot-initargs :test #'eq)
             (let ((index (slot-definition-location slotd)))
               (if (mkcl:fixnump index)
                   (si:instance-set instance (the fixnum index) val) ;; local direct. JCB
                 (rplaca index val)))  ;; shared direct. JCB
	     (return t)))
       ;; Try to initialize the slot from its initform.
       (when (and slot-names
		  (or (eq slot-names 'T)
		      (member slot-name slot-names))
		  (not (slot-boundp instance slot-name)))
	 (let ((initfun (slot-definition-initfunction slotd)))
	   (when initfun
             (let ((index (slot-definition-location slotd))
                   (val (funcall initfun)))
               (if (mkcl:fixnump index)
                   (si:instance-set instance (the fixnum index) val) ;; local direct. JCB
                 (rplaca index val)))  ;; shared direct. JCB
	     ))))))
  instance)

;;; ----------------------------------------------------------------------
;;; CLASSES INITIALIZATION AND REINITIALIZATION
;;;

(defun compute-instance-size (slots)
  (loop for slotd in slots
     with last-location = 0
     with num-slots = 0
     when (eq (slot-definition-allocation slotd) :instance)
     do (let ((new-loc (safe-slot-definition-location slotd)))
	  (incf num-slots)
	  (when (and new-loc (> new-loc last-location))
	    (setf last-location new-loc)))
     finally (return (max num-slots (1+ last-location)))))

(defmethod allocate-instance ((class class) &rest initargs)
  (declare (ignore initargs))
  ;; FIXME! Inefficient! We should keep a list of dependent classes.
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((cached-class-size +cached-class-size+))
    (if (and cached-class-size (eq class +cached-class+))
	(let ((x (si::allocate-raw-instance nil class cached-class-size)))
          (declare (type standard-object x))
	  (si::instance-sig-set2 x +cached-class-slots+)
	  x)
      (let ((x (si::allocate-raw-instance nil class (class-size class))))
        (declare (type standard-object x))
	(si::instance-sig-set x)
	x)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original make-instance code. 2010/08/19 JCB

;; (defmethod make-instance ((class class) &rest initargs &key &allow-other-keys)
;;   ;; Without finalization we can not find initargs.
;;   (unless (class-finalized-p class)
;;     (finalize-inheritance class))
;;   ;; We add the default-initargs first, because one of these initargs might
;;   ;; be (:allow-other-keys t), which disables the checking of the arguments.
;;   ;; (Paul Dietz's ANSI test suite, test CLASS-24.4)
;;   (setf initargs (add-default-initargs class initargs))
;;   (check-initargs class initargs
;; 		  (append (compute-applicable-methods
;; 			   #'allocate-instance (list class))
;; 			  (compute-applicable-methods
;; 			   #'initialize-instance (list (class-prototype class)))
;; 			  (compute-applicable-methods
;; 			   #'shared-initialize (list (class-prototype class) t))))
;;   (let ((instance (apply #'allocate-instance class initargs)))
;;     (apply #'initialize-instance instance initargs)
;;     instance))

(defun compute-make-instance-function (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((allocate-instance-methods (compute-applicable-methods
				     #'allocate-instance (list class)))
	 (initialize-instance-methods (compute-applicable-methods
				       #'initialize-instance (list (class-prototype class))))
	 (shared-initialize-methods (compute-applicable-methods
				     #'shared-initialize (list (class-prototype class) t)))
	 (all-relevant-methods (append allocate-instance-methods 
				       initialize-instance-methods
				       shared-initialize-methods))
	 (allocate-instance-emfun
	  (compute-effective-method #'allocate-instance
				    (generic-function-method-combination #'allocate-instance)
				    allocate-instance-methods))
	 (initialize-instance-emfun
	  (compute-effective-method #'initialize-instance
				    (generic-function-method-combination #'initialize-instance)
				    initialize-instance-methods))
	 (cached-shared-initialize-emfun
	  (compute-effective-method #'shared-initialize
				    (generic-function-method-combination #'shared-initialize)
				    shared-initialize-methods))
	 (cached-class-slots (class-slots class))
	 (cached-class-size (class-size class))
	 (cached-class class)
	 all-method-initargs
	 allow-other-keys
	 )
    (multiple-value-setq (all-method-initargs allow-other-keys)
      (valid-keywords-from-methods all-relevant-methods))

    #'(lambda (class initargs)
	(if (eq cached-class-slots (class-slots class))
	    ;; cache hit
	    (progn
	      ;; We add the default-initargs first, because one of these initargs might
	      ;; be (:allow-other-keys t), which disables the checking of the arguments.
	      ;; (Paul Dietz's ANSI test suite, test CLASS-24.4)
	      (setf initargs (add-default-initargs class initargs))
	      (unless allow-other-keys
		(check-initargs class initargs all-method-initargs))
	      (let ((+inside-make-instance+ t)
		    (+cached-shared-initialize-emfun+ cached-shared-initialize-emfun)
		    (+cached-class-slots+ cached-class-slots)
		    (+cached-class-size+ cached-class-size)
		    (+cached-class+ cached-class)
		    )
		(declare (special +inside-make-instance+))
		(let* ((si:*dynamic-cons-stack* si:*dynamic-cons-stack*)
		       (instance (funcall allocate-instance-emfun (si:dyn-list* class initargs) nil))
		       (+instance-to-initialize+ instance)
		       )
		  (funcall initialize-instance-emfun (si:dyn-list* instance initargs) nil)
		  instance))
	      )
	  ;; cache miss
	  (let ((cached-make-instance (with-metadata-lock
				       (setf (class-cached-make-instance class)
					     (compute-make-instance-function class)))))
	    (funcall cached-make-instance class initargs)
	    )
	  )
	)
    )
  )
	

(defmethod make-instance ((class class) &rest initargs)
  (declare (dynamic-extent initargs))
  (let ((cached-make-instance (class-cached-make-instance class)))
    (unless (and cached-make-instance (class-finalized-p class))
      (with-metadata-lock
       (setq cached-make-instance
	     (setf (class-cached-make-instance class)
		   (compute-make-instance-function class))))
      )
    (funcall cached-make-instance class initargs)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-default-initargs (class initargs)
  ;; Here, for each slot which is not mentioned in the initialization
  ;; arguments, but which has a value associated with :DEFAULT-INITARGS,
  ;; we compute the value and add it to the list of initargs.
  (dolist (scan (class-default-initargs class))
    (let* ((initarg (first scan))
	   (value (third scan))
	   (supplied-value (si::search-keyword initargs initarg)))
      (when (or (eq supplied-value '+initform-unsupplied+)
		(eq supplied-value 'si::failed))
	(when (eq supplied-value '+initform-unsupplied+)
	  (remf initargs initarg))
	(setf value (funcall value)
	      initargs (append initargs (list initarg value))))))
  initargs)

(defmethod direct-slot-definition-class ((class T) &rest canonicalized-slot)
  (declare (ignore canonicalized-slot))
  (find-class 'standard-direct-slot-definition nil))

(defmethod effective-slot-definition-class ((class T) &rest canonicalized-slot)
  (declare (ignore canonicalized-slot))
  (find-class 'standard-effective-slot-definition nil))

(defun has-forward-referenced-parents (class)
  (or (forward-referenced-class-p class)
      (and (not (class-finalized-p class))
           (some #'has-forward-referenced-parents
                 (class-direct-superclasses class)))))

(defun finalize-unless-forward (class)
  (unless (has-forward-referenced-parents class)
    (finalize-inheritance class)))



(defmethod initialize-instance ((class class) &rest initargs &key sealedp direct-superclasses direct-slots)
  (declare (ignore sealedp))
  (declare (dynamic-extent initargs))
  ;; convert the slots from lists to direct slots
  (setf direct-slots (loop for s in direct-slots
			   collect (canonical-slot-to-direct-slot class s)))

  ;; verify that the inheritance list makes sense
  (setf direct-superclasses (check-direct-superclasses class direct-superclasses))

  (apply #'call-next-method class
         :direct-slots direct-slots
         :direct-superclasses direct-superclasses
         initargs)

  ;; record the inheritance in parents
  (dolist (l (setf direct-superclasses (class-direct-superclasses class)))
    (add-direct-subclass l class))

  (finalize-unless-forward class) ;; somewhat needlessly eager. JCB

  class)

(defvar *optimize-slot-access* t)

(defmethod shared-initialize :after ((class standard-class) slot-names &rest initargs
                                     &key
				     (optimize-slot-access (list *optimize-slot-access*))
				     sealedp)
  (declare (ignore slot-names initargs))
  (setf (class-optimize-slot-access class) (first optimize-slot-access)
	(class-sealedp class) (and sealedp t)))

(defmethod add-direct-subclass ((parent class) child)
  (pushnew child (class-direct-subclasses parent)))

(defmethod remove-direct-subclass ((parent class) child)
  (setf (class-direct-subclasses parent)
	(remove child (class-direct-subclasses parent))))

(defmethod check-direct-superclasses (class supplied-superclasses)
  (unless supplied-superclasses
    (setf supplied-superclasses
	  (list (find-class (typecase class
			      (STANDARD-CLASS 'STANDARD-OBJECT)
			      (STRUCTURE-CLASS 'STRUCTURE-OBJECT)
			      (otherwise (error "No :DIRECT-SUPERCLASS argument was supplied ~
                                                 for metaclass ~S." (class-of class))))))))
  ;; FIXME!!! Here should come the invocation of VALIDATE-SUPERCLASS!
  ;; FIXME!!! We should check that structures and standard objects are
  ;; not mixed, and that STANDARD-CLASS, or STANDARD-GENERIC-FUNCTION,
  ;; etc, are the first classes.
  supplied-superclasses)

;;; ----------------------------------------------------------------------
;;; FINALIZATION OF CLASS INHERITANCE
;;;
(defun forward-referenced-class-p (x)
  (let ((y (find-class 'FORWARD-REFERENCED-CLASS nil)))
    (and y (si::subclassp (class-of x) y))))


(defvar *trace-finalize-inheritance* nil)

(defmethod finalize-inheritance ((class class))
  ;; FINALIZE-INHERITANCE computes the guts of what defines a class: the
  ;; slots, the list of parent class, etc. It is called when either the
  ;; class was not finalized before, or when one of the parents has been
  ;; modified.
  ;;

  (when *trace-finalize-inheritance*
    (format t "~&In finalize-inheritance for class: ~S." class))

  ;; A class can be finalized only if all of its parents are finalized. JCB
  (dolist (super (class-direct-superclasses class))
    (unless (class-finalized-p super)
      (finalize-inheritance super)))
  
  (let ((cpl (compute-class-precedence-list class)))

    (setf (class-precedence-list class) cpl)

    (let ((slots (compute-slots class)))
      (setf (class-slots class) slots
	    (class-size class) (compute-instance-size slots)
	    (class-default-initargs class) (compute-default-initargs class)
	    (class-finalized-p class) t))

#|
    ;;
    ;; When a class is sealed we rewrite the list of direct slots to fix
    ;; their locations. This may imply adding _new_ direct slots.
    ;;
    (when (class-sealedp class)
      (let* ((free-slots (delete-duplicates (mapcar #'slot-definition-name (class-slots class)))))
	;;
	;; We first search all slots that belonged to unsealed classes and which
	;; therefore have no fixed position.
	;;
	(loop for c in cpl
	   do (loop for slotd in (class-direct-slots c)
		 when (safe-slot-definition-location slotd)
		 do (setf free-slots (delete (slot-definition-name slotd) free-slots))))
	;;
	;; We now copy the locations of the effective slots in this class to
	;; the class direct slots.
	;;
	(loop for slotd in (class-direct-slots class)
	   do (let ((name (slot-definition-name slotd)))
		(setf (slot-definition-location slotd)
		      (slot-definition-location (find-slot-definition class name))
		      free-slots (delete name free-slots))))
	;;
	;; And finally we add one direct slot for each inherited slot that did
	;; not have a fixed location.
	;;
	(loop for name in free-slots
	   with direct-slots = (class-direct-slots class)
	   do (let* ((effective-slotd (find-slot-definition class name))
		     (def (loop for (name . rest) in +slot-definition-slots+
			     nconc (list (getf rest :initarg)
					 (funcall (getf rest :accessor) effective-slotd)))))
		(push (apply #'make-instance (direct-slot-definition-class class def)
			     def)
		      direct-slots))
	   finally (setf (class-direct-slots class) direct-slots))))
|#
    ;;
    ;; This is not really needed, because when we modify the list of slots
    ;; all instances automatically become obsolete (See change.lsp)
    ;(make-instances-obsolete class)
    ;;
    ;; But this is really needed: we have to clear the different type caches
    ;; for type comparisons and so on.
    ;;
    (si::subtypep-clear-cache)
    )
  )

(defun std-create-slots-table (class)
  (let* ((all-slots (class-slots class))
	 (table (make-hash-table :size (max 32 (* 2 (length all-slots))))))
    (dolist (slotd (class-slots class))
      (setf (gethash (slot-definition-name slotd) table) slotd))
    (setf (slot-table class) table)))

(defmethod finalize-inheritance ((class standard-class))
  (call-next-method)
  (std-create-slots-table class)
  (std-class-generate-accessors class))

(defmethod compute-class-precedence-list ((class class))
  (compute-clos-class-precedence-list class (class-direct-superclasses class)))

(defmethod compute-slots ((class class))
  ;; INV: for some classes MKCL expects that the order of the inherited slots is
  ;; preserved. The following code ensures that, if C1 is after C2 in the
  ;; class precedence list, and the slot S1 appears both in C1 and C2,
  ;; the slot S1 will appear the new class before the slots of C2; and
  ;; whenever possible, in the same position as in C1.
  ;;
  (do* ((all-slots (mapappend #'class-direct-slots (reverse (class-precedence-list class))))
	(all-names (nreverse (mapcar #'slot-definition-name all-slots)))
	(output '())
	(scan all-names (cdr scan)))
       ((endp scan) output)
    (let ((name (first scan)))
      (unless (find name (rest scan))
	(push (compute-effective-slot-definition
	       class name (delete name (reverse all-slots) :key #'slot-definition-name
				  :test-not #'eq))
	      output)))))

(defun slot-definition-to-plist (slotd)
  (list :name (slot-definition-name slotd)
	:initform (slot-definition-initform slotd)
	:initfunction (slot-definition-initfunction slotd)
	:type (slot-definition-type slotd)
	:allocation (slot-definition-allocation slotd)
	:initargs (slot-definition-initargs slotd)
	:readers (slot-definition-readers slotd)
	:writers (slot-definition-writers slotd)
	:documentation (slot-definition-documentation slotd)
	:location (slot-definition-location slotd)))

(defun safe-slot-definition-location (slotd &optional default)
  (if (or (listp slotd) (slot-boundp slotd 'location))
      (slot-definition-location slotd)
    default))

(defmethod compute-effective-slot-definition ((class class) name direct-slots)
  (declare (ignorable name))
  (flet ((direct-to-effective (old-slot)
	   (if (consp old-slot)
	       (copy-list old-slot)
	       (let ((initargs (slot-definition-to-plist old-slot)))
		 (apply #'make-instance
			(apply #'effective-slot-definition-class class initargs)
			initargs))))
	 (combine-slotds (new-slotd old-slotd)
	   (let* ((new-type (slot-definition-type new-slotd))
		  (old-type (slot-definition-type old-slotd))
		  ;;(loc1 (safe-slot-definition-location new-slotd))
		  ;;(loc2 (safe-slot-definition-location old-slotd))
                  )
             ;; The most specific :allocation and :location always mask anything else period.
             ;; There is no further "inheritance" of any kind on this, so no need to mess with it! JCB
#|
	     (when loc2
	       (if loc1
		   (unless (eql loc1 loc2)
		     (error 'simple-error
			    :format-control "You have specified two conflicting ~
                                             slot locations:~%~D and ~F~%for slot ~A"
			    :format-arguments (list loc1 loc2 name)))
		   (progn
		     #+(or)
		     (format t "~%Assigning a default location ~D for ~A in ~A."
			     loc2 name (class-name class))
		     (setf (slot-definition-location new-slotd) loc2))))
|#
	     (setf (slot-definition-initargs new-slotd)
		   (union (slot-definition-initargs new-slotd)
			  (slot-definition-initargs old-slotd)))
	     (unless (slot-definition-initfunction new-slotd)
	       (setf (slot-definition-initform new-slotd)
		     (slot-definition-initform old-slotd)
		     (slot-definition-initfunction new-slotd)
		     (slot-definition-initfunction old-slotd)))
	     (setf (slot-definition-readers new-slotd)
		   (union (slot-definition-readers new-slotd)
			  (slot-definition-readers old-slotd))
		   (slot-definition-writers new-slotd)
		   (union (slot-definition-writers new-slotd)
			  (slot-definition-writers old-slotd))
		   (slot-definition-type new-slotd)
		   ;; FIXME! we should be more smart than this:
		   (cond ;; Wrong anyway! ((subtypep new-type old-type) new-type)
			 ;; Wrong anyway! ((subtypep old-type new-type) old-type)
                         ((eq new-type t) old-type)
                         ((eq old-type t) new-type)
			 (T `(and ,new-type ,old-type)))) ;; This one is the only one that is by the spec. JCB
	     new-slotd)))
    (reduce #'combine-slotds (rest direct-slots)
	    :initial-value (direct-to-effective (first direct-slots)))))

(defmethod compute-default-initargs ((class class))
  (let ((all-initargs (mapappend #'class-direct-default-initargs
				 (class-precedence-list class))))
    ;; We have to use this trick because REMOVE-DUPLICATES on
    ;; ((:foo x) (:faa y) (:foo z)) would produce ((:faa y) (:foo z))
    ;; and we want ((:foo x) (:faa y))
    (nreverse (remove-duplicates (reverse all-initargs) :key #'first))))


;;; ======================================================================
;;; STANDARD-CLASS specializations
;;;
;;; IMPORTANT: The following implementation of ENSURE-CLASS-USING-CLASS is
;;; shared by the metaclasses STANDARD-CLASS and STRUCTURE-CLASS.
;;;

(defun migrate-spec-users (old-class new-class)
  (dolist (method (class-spec-users old-class))
    (when (member :method *warn-on-redefinition*)
      (warn 'mkcl::simple-style-warning
	    :format-control "~&Method ~S migrated from: ~S, to: ~S.~%"
	    :format-arguments 
	    (list (generic-function-name (method-generic-function method)) old-class new-class)))

    (migrate-method method old-class new-class)
    )
  )

(defun redefine-subclasses (old-class new-class)
  ;; Should we redefine only PROPERLY named subclasses? (After some thought I think NOT.) JCB.
  (flet ((rebuild-slot-list (slots)
	   (loop for slot in slots
		 collect (slot-definition-to-plist slot))))
    (dolist (subclass (class-direct-subclasses old-class))
      (ensure-class (class-name subclass)
		    :direct-superclasses (substitute new-class old-class
						     (class-direct-superclasses subclass))
		    :direct-slots (rebuild-slot-list (class-direct-slots subclass))
		    :direct-default-initargs (class-direct-default-initargs subclass)
		    :documentation (class-documentation subclass)
		    :source (class-source subclass)
		    :version (1+ (class-version subclass))
		    :previous subclass
		    :metaclass (class-of subclass)
		    ))))

;;(defvar *redefine-class-in-place* t) ;; moved to symbols_list.h
(defvar *migrate-methods-on-class-redefinition* nil)
(defvar *migrate-subclasses-on-class-redefinition* nil)

(defmethod ensure-class-using-class ((class forward-referenced-class) name &rest rest
				     &key direct-slots direct-default-initargs &allow-other-keys)
  (declare (ignore direct-slots direct-default-initargs))
  ;;(declare (dynamic-extent rest))
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (change-class class metaclass)
    (apply #'reinitialize-instance class :name name options)))

(defmethod ensure-class-using-class ((class class) name &rest rest
				     &key direct-slots direct-default-initargs &allow-other-keys)
  (declare (ignore direct-slots direct-default-initargs))
  ;;(declare (dynamic-extent rest))
  (clear-cached-make-instance class)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (cond #+(or) ((forward-referenced-class-p class)
		  (change-class class metaclass))
	  ((not (eq (class-of class) metaclass))
	   (error "When redefining a class, the metaclass can not change.")))
    (when (member :class *warn-on-redefinition*)
      (warn 'mkcl::simple-style-warning
	    :format-control "~&Redefining class ~A.~%   ~
                               Previous definition location: ~S.~%   ~
                               New definition location: ~S.~%"
	    :format-arguments (list name (class-source class) (getf options :source))))

    #+mkcl-min
    (return-from ensure-class-using-class class) ;; No redefinition during bootstrap. JCB

    (if *redefine-class-in-place*
	(apply #'reinitialize-instance class :name name options)
      (let ((new-class (apply #'make-instance metaclass
			      :name name :previous class
			      :version (1+ (class-version class))
			      options)))
	(when *migrate-subclasses-on-class-redefinition*
	  (redefine-subclasses class new-class))
	(when *migrate-methods-on-class-redefinition*
	  (migrate-spec-users class new-class))
	new-class
	)
      )))

(defun coerce-to-class (class-or-symbol &optional (fail nil) &key options)
  (cond ((si:instancep class-or-symbol) class-or-symbol)
	((not (symbolp class-or-symbol))
	 (error "~a is not a valid class specifier." class-or-symbol))
	((find-class class-or-symbol fail))
	(t
	 (warn 'mkcl::simple-style-warning
	       :format-control "Class ~A has been forward referenced."
	       :format-arguments (list class-or-symbol))
	 (ensure-class class-or-symbol
		       :metaclass 'forward-referenced-class
		       :direct-superclasses (list (find-class 'standard-object))
		       :direct-slots '()
		       :source (getf options :source)))))

(defun help-ensure-class (&rest options
			  &key (metaclass 'standard-class) direct-superclasses
			  &allow-other-keys)
  (remf options :metaclass)
  (remf options :direct-superclasses)
  (setf metaclass (coerce-to-class metaclass t)
	direct-superclasses (mapcar #'(lambda (x) (coerce-to-class x nil :options options))
				    direct-superclasses))
  (values metaclass direct-superclasses
	  (list* :direct-superclasses direct-superclasses options)))

;;; ----------------------------------------------------------------------
;;; Around methods for COMPUTE-SLOTS which assign locations to each slot.
;;;

(defun class-compute-slots (class slots)
  ;; This an MKCL extension. We are allowed to specify the location of
  ;; a direct slot. Consequently we have to first sort the ones which
  ;; have been predefined and then assign locations _after_ the last
  ;; assigned slot. Note the generalized comparison, which pushes all
  ;; slots without a defined location to the end of the list.
  (let* ((size (compute-instance-size slots))
	 (instance-slots (remove :instance slots :key #'slot-definition-allocation
						 :test-not #'eq))
	 (numbered-slots (remove-if-not #'safe-slot-definition-location instance-slots))
	 (other-slots (remove-if #'safe-slot-definition-location instance-slots))
	 (aux (make-array size :element-type 't :adjustable nil :initial-element nil)))
    (loop for i in numbered-slots
       do (let ((loc (slot-definition-location i)))
	    (when (aref aux loc)
	      (error 'simple-error
		     :format-control "Slots ~A and ~A are said to have the same location in class ~A."
		     :format-ars (list (aref aux loc) i class)))
	    (setf (aref aux loc) i)))
    (loop for i in other-slots
       with index = 0
       do (loop while (aref aux index)
	       do (incf index)
	       finally (setf (aref aux index) i
			     (slot-definition-location i) index)))
    slots))

(defmethod compute-slots :around ((class class))
  (class-compute-slots class (call-next-method)))

(defun std-class-compute-slots (class slots)
  #+(and) (declare (ignore class))
  #+(or)
  (let* ((direct-slots (class-direct-slots class)))
    (dolist (slotd slots)
      (let* ((name (slot-definition-name slotd))
	     (allocation (slot-definition-allocation slotd)))
	;; This whole shared slot initialization done here is for the birds! JCB
	#+(or)
	(cond ((not (eq (slot-definition-allocation slotd) :class)))
	      ((find name direct-slots :key #'slot-definition-name) ; new shared slot
	       (setf (slot-definition-location slotd) (list (unbound)))
	       (format t "~&Initialized slot ~S to value ~S.~%" name (slot-definition-location slotd))
	       )
	      (t			; inherited shared slot
	       (dolist (c (class-precedence-list class))
		 (unless (eql c class)
		   (let ((other (find (slot-definition-name slotd)
				      (class-slots c)
				      :key #'slot-definition-name)))
		     (when (and other
				(eq (slot-definition-allocation other) allocation)
				(setf (slot-definition-location slotd)
				      (slot-definition-location other)))
		       (return)))))))
	)))
    slots)

(defmethod compute-slots :around ((class standard-class))
  (std-class-compute-slots class (call-next-method)))

;;; ----------------------------------------------------------------------
;;; Optional accessors
;;;

(defun unbound-slot-error (object index)
  (declare (type standard-object object) (type fixnum index))
  (let* ((class (class-of object))
	 (slotd (find index (class-slots class) :key #'slot-definition-location)))
    (values (slot-unbound class object (slot-definition-name slotd)))))

(defun safe-instance-ref (object index)
  (declare (type standard-object object) (type fixnum index))
  (ensure-up-to-date-instance object)
  (let ((value (si:instance-ref object index)))
    (if (si:sl-boundp value)
	value
      (unbound-slot-error object index))))

;;; The following does not get as fast as it should because we are not
;;; allowed to memoize the position of a slot. The problem is that the
;;; AMOP specifies that slot accessors are created from the direct
;;; slots, without knowing the slot position. This semantics is
;;; required for working standard-reader- and
;;; standard-writer-method. OTOH if we want to have memoized slot
;;; positions we have to work from the effective slots and we have to
;;; create methods for all slots, not only the direct ones in this
;;; class. Both semantics are incompatible, but we currently have no
;;; safe way to choose one or another
;;;
(defun std-class-optimized-accessors (slotd)
  (macrolet ((slot-definition-location (slotd)
	       `(si::instance-ref ,slotd #.(position 'location +slot-definition-slots+ :key #'first))))
    (values #'(lambda (self)
                (declare (type standard-object self))
                (ensure-up-to-date-instance self)
		(let* ((index (slot-definition-location slotd))
		       (value (if (mkcl:fixnump index)
				  (si:instance-ref self (the fixnum index))
				  (car index)))) ;; the assumption that index is a cons is not very safe. JCB
		  (if (si:sl-boundp value)
		      value
		      (values (slot-unbound (si::instance-class self) self (slot-definition-name slotd))))))
	    #'(lambda (value self)
                (declare (type standard-object self))
                (ensure-up-to-date-instance self)
                (let ((index (slot-definition-location slotd)))
		  (if (mkcl:fixnump index)
		      (si:instance-set self (the fixnum index) value)
		      (setf (car index) value))))))) ;; the assumption that index is a cons is not very safe. JCB

(defun std-class-optimized-local-slot-accessors (index)
  (declare (type fixnum index))
  (values #'(lambda (self)
              (declare (type standard-object self))
              (ensure-up-to-date-instance self)
              (safe-instance-ref self (the fixnum index)))
	  #'(lambda (value self)
              (declare (type standard-object self))
              (ensure-up-to-date-instance self)
              (si:instance-set self (the fixnum index) value))))

(defun std-class-accessors (slotd)
  ;; The following are very slow. It does the full MOP instance structure protocol dance.
  (values #'(lambda (self)
	      (slot-value-using-class (si::instance-class self) self slotd))
	  #'(lambda (value self)
	      (setf (slot-value-using-class (si::instance-class self) self slotd) value))))

(defun std-class-generate-accessors (standard-class &aux optimizable)
  (dolist (slotd (class-slots standard-class))
    (multiple-value-bind (reader writer)
	(let ((allocation (slot-definition-allocation slotd))
	      (location (safe-slot-definition-location slotd)))
	  (cond ((class-optimize-slot-access standard-class)
                 (if (and (eq allocation :instance) (typep location 'fixnum))
                     (std-class-optimized-local-slot-accessors location)
                   (std-class-optimized-accessors slotd)))
		(t
		 (std-class-accessors slotd))))
      (let* ((reader-args (list :function reader
				:generic-function nil
				:qualifiers nil
				:lambda-list '(object)
				:specializers `(,standard-class)
				:slot-definition slotd))
	     (reader-class (if (boundp '*early-methods*)
			       'standard-reader-method
                             (apply #'reader-method-class standard-class slotd reader-args)))
	     (writer-args (list :function writer
				:generic-function nil
				:qualifiers nil
				:lambda-list '(value object)
				:specializers `(,(find-class t) ,standard-class)
				:slot-definition slotd))
	     (writer-class (if (boundp '*early-methods*)
			       'standard-writer-method
			     (apply #'writer-method-class standard-class slotd writer-args))))
	(dolist (fname (slot-definition-readers slotd))
	  (install-method fname nil `(,standard-class) '(self)
			  nil nil reader nil reader-class
			  :slot-definition slotd
			  :source (class-source standard-class)))
	(dolist (fname (slot-definition-writers slotd))
	  (install-method fname nil `(,(find-class t) ,standard-class) '(value self)
			  nil nil writer nil writer-class
			  :slot-definition slotd
			  :source (class-source standard-class)))))))

;;; ======================================================================
;;; STANDARD-OBJECT
;;;
;;; Standard-object has no slots and inherits only from t:
;;; (defclass standard-object (t) ())

(defmethod describe-object ((obj standard-object) (stream t))
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class))
	 slotname has-shared-slots)
    (format stream "~%~S is an instance of class ~S" obj class)
    (when slotds
      ;; print instance slots
      (format stream "~%it has the following instance slots")
      (dolist (slot slotds)
	(setq slotname (slot-definition-name slot))
	(case (slot-definition-allocation slot)
	  (:INSTANCE
           (ignore-errors
             (format stream "~%~A:~24,8T~A"
                     slotname
                     (if (slot-boundp obj slotname)
                         (slot-value obj slotname) "Unbound"))))
	  ;; :CLASS
	  (T (setq has-shared-slots t))))
      (when has-shared-slots
	;; print class slots
	(format stream "~%it has the following class slots")
	(dolist (slot slotds)
	  (setq slotname (slot-definition-name slot))
	  (unless (eq (slot-definition-allocation slot) :INSTANCE)
            (ignore-errors
              (format stream "~%~A:~24,8T~A"
                      slotname
                      (if (slot-boundp obj slotname)
                          (slot-value obj slotname) "Unbound"))))))))
  obj)

;;; ----------------------------------------------------------------------
;;; Methods

#-(and) ;; This buys us next to nothing over the standard-object method and is more fragile. JCB
(defmethod describe-object ((obj standard-class) (stream t))
  (ensure-up-to-date-instance obj)
  (let ((slotds (class-slots (si:instance-class obj))))
    (ignore-errors
      (format t "~%~A is an instance of class ~A"
              obj (class-name (si:instance-class obj))))
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i)))
	((null scan))
      (declare (fixnum i))
      (print (slot-definition-name (car scan))) (princ ":	")
      (case (slot-definition-name (car scan))
	    ((SUPERIORS INFERIORS PRECEDENCE-LIST)
	     (princ "(")
	     (do* ((scan (si:instance-ref obj i) (cdr scan))
		   (e (car scan) (car scan)))
		  ((null scan))
		  (prin1 (class-name e))
		  (when (cdr scan) (princ " ")))
	     (princ ")"))
	    (otherwise (ignore-errors (prin1 (si:instance-ref obj i)))))))
  obj)

