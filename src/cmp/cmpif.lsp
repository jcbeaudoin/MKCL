;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2017, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; CMPIF  Conditionals.

(in-package "COMPILER")

(defun c1if (args &aux (*load-control-flow-is-linear* nil))
  (check-args-number 'IF args 2 3)
  (let ((f (c1fmla-constant (car args))))
    (case f
      ((T) (c1expr (second args)))
      ((NIL) (if (endp (cddr args)) (c1nil) (c1expr (third args))))
      (otherwise
       (make-c1form* 'IF :args (c1fmla f) (c1expr (second args))
		     (if (endp (cddr args)) (c1nil) (c1expr (third args))))))
    ))

(defun c1fmla-constant (fmla &aux f)
  (cond
   ((consp fmla)
    (case (car fmla)
          (AND (do ((fl (cdr fmla) (cdr fl)))
                   ((endp fl) t)
		 (setq f (c1fmla-constant (car fl)))
		 (case f
		   ((T))
		   ((NIL) (return nil))
		   (t (if (endp (cdr fl))
			  (return f)
			  (return (list* 'AND f (cdr fl))))))))
          (OR (do ((fl (cdr fmla) (cdr fl)))
                  ((endp fl) nil)
		(setq f (c1fmla-constant (car fl)))
		(case f
		  ((T) (return t))
		  ((NIL))
		  (t (if (endp (cdr fl))
			 (return f)
			 (return (list* 'OR f (cdr fl))))))))
          ((NOT NULL)
           (when (endp (cdr fmla)) (too-few-args 'not 1 0))
           (unless (endp (cddr fmla))
                   (too-many-args 'not 1 (length (cdr fmla))))
           (setq f (c1fmla-constant (second fmla)))
           (case f
                 ((T) nil)
                 ((NIL) t)
                 (t (list 'NOT f))))
          (t fmla)))
   ((symbolp fmla) (if (constantp fmla)
                       (if (symbol-value fmla) t nil)
                       fmla))
   (t t))
  )

(defun c1fmla (fmla)
  (if (consp fmla)
      (case (car fmla)
            (AND (case (length (cdr fmla))
                   (0 (c1t))
                   (1 (c1fmla (second fmla)))
                   (t (apply #'make-c1form* 'FMLA-AND :args
			     (mapcar #'c1fmla (rest fmla))))))
            (OR (case (length (cdr fmla))
                   (0 (c1nil))
                   (1 (c1fmla (second fmla)))
                   (t (apply #'make-c1form* 'FMLA-OR :args
			     (mapcar #'c1fmla (rest fmla))))))
            ((NOT NULL)
	     (check-args-number 'NOT (rest fmla) 1 1)
	     (make-c1form* 'FMLA-NOT :args (c1fmla (second fmla))))
            (t (c1expr fmla)))
      (c1expr fmla))
  )

(defun c2if (fmla form1 form2
                  &aux (Tlabel (next-label)) Flabel)
  (cond ((and (eq (c1form-name form2) 'LOCATION)
              (null (c1form-arg 0 form2))
              (eq *destination* 'TRASH)
              (not (eq *exit* 'RETURN)))
         (let* ((exit *exit*)
		(*unwind-exit* (cons Tlabel *unwind-exit*))
		(*exit* Tlabel))
	   (CJF fmla Tlabel exit))
         (wt-label Tlabel)
         (c2expr form1))
        (t
         (setq Flabel (next-label))
         (let ((*unwind-exit* (cons Flabel (cons Tlabel *unwind-exit*)))
               (*exit* Tlabel))
              (CJF fmla Tlabel Flabel))
         (wt-label Tlabel)
         (let ((*unwind-exit* (cons 'JUMP *unwind-exit*))
	       (*temp* *temp*))
	   (c2expr form1))
         (wt-label Flabel)
         (c2expr form2)))
  )

;;; If fmla is true, jump to Tlabel.  If false, do nothing.
(defun CJT (fmla Tlabel Flabel)
  (case (c1form-name fmla)
    (FMLA-AND (do ((fs (c1form-args fmla) (cdr fs)))
                  ((endp (cdr fs))
                   (CJT (car fs) Tlabel Flabel))
                (let* ((label (next-label))
                       (*unwind-exit* (cons label *unwind-exit*)))
                  (CJF (car fs) label Flabel)
                  (wt-label label))))
    (FMLA-OR (do ((fs (c1form-args fmla) (cdr fs)))
                 ((endp (cdr fs))
                  (CJT (car fs) Tlabel Flabel))
               (let* ((label (next-label))
                      (*unwind-exit* (cons label *unwind-exit*)))
                 (CJT (car fs) Tlabel label)
                 (wt-label label))))
    (FMLA-NOT (CJF (c1form-arg 0 fmla) Flabel Tlabel))
    (LOCATION
     (case (first (c1form-args fmla))
       ((T) (unwind-no-exit Tlabel) (wt-nl) (wt-go Tlabel))
       ((NIL))
       (t (let ((*destination* (list 'JUMP-TRUE Tlabel)))
            (c2expr* fmla)))))
    (t (let ((*destination* (list 'JUMP-TRUE Tlabel))) (c2expr* fmla))))
  )

;;; If fmla is false, jump to Flabel.  If true, do nothing.
(defun CJF (fmla Tlabel Flabel)
  (case (c1form-name fmla)
    (FMLA-AND (do ((fs (c1form-args fmla) (cdr fs)))
                  ((endp (cdr fs)) (CJF (car fs) Tlabel Flabel))
                (declare (object fs))
                (let* ((label (next-label))
                       (*unwind-exit* (cons label *unwind-exit*)))
                  (CJF (car fs) label Flabel)
                  (wt-label label))))
    (FMLA-OR (do ((fs (c1form-args fmla) (cdr fs)))
                 ((endp (cdr fs)) (CJF (car fs) Tlabel Flabel))
               (declare (object fs))
               (let* ((label (next-label))
                      (*unwind-exit* (cons label *unwind-exit*)))
                 (CJT (car fs) Tlabel label)
                 (wt-label label))))
    (FMLA-NOT (CJT (c1form-arg 0 fmla) Flabel Tlabel))
    (LOCATION
     (case (first (c1form-args fmla))
       ((T))
       ((NIL) (unwind-no-exit Flabel) (wt-nl) (wt-go Flabel))
       (t (let ((*destination* (list 'JUMP-FALSE Flabel)))
            (c2expr* fmla)))))
    (t (let ((*destination* (list 'JUMP-FALSE Flabel))) (c2expr* fmla))))
  )

(defun set-jump-true (loc label)
  (cond ((null loc))
	((eq loc t)
	 (unwind-no-exit label)
	 (wt-nl) (wt-go label))
	(t
	 (cond ((eq (loc-representation-type loc) :bool)
		(wt-nl "if(" loc "){"))
	       (t
		(wt-nl "if((")
		(wt-coerce-loc :object loc)
		(wt ")!=mk_cl_Cnil){")))
	 (unwind-no-exit label)
	 (wt-nl) (wt-go label)
	 (wt "}"))))

(defun set-jump-false (loc label)
  (cond ((eq loc t))
	((null loc)
	 (unwind-no-exit label)
	 (wt-nl) (wt-go label))
	(t
	 (cond ((eq (loc-representation-type loc) :bool)
		(wt-nl "if(!(" loc ")){"))
	       (t
		(wt-nl "if((")
		(wt-coerce-loc :object loc)
		(wt ")==mk_cl_Cnil){")))
	 (unwind-no-exit label)
	 (wt-nl) (wt-go label)
	 (wt "}"))))

;;; ----------------------------------------------------------------------

(put-sysprop 'if 'c1special #'c1if)
(put-sysprop 'if 'c2 #'c2if)
(put-sysprop 'jump-true 'set-loc #'set-jump-true)
(put-sysprop 'jump-false 'set-loc #'set-jump-false)

