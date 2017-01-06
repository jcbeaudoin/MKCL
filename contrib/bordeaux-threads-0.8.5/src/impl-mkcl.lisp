;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil
Copyright 2010 Jean-Claude Beaudoin.

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(deftype thread ()
  'mt:thread)

;;; Thread Creation

(defun %make-thread (function name)
  (mt:thread-run-function name function))
(declaim (notinline %make-thread))

(defun current-thread ()
  mt::*thread*)
(declaim (notinline current-thread))

(defun threadp (object)
  (typep object 'mt:thread))
(declaim (notinline threadp))

(defun thread-name (thread)
  (mt:thread-name thread))
(declaim (notinline thread-name))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (mt:make-lock :name (or name "Anonymous lock")))
(declaim (notinline make-lock))

(defun acquire-lock (lock &optional (wait-p t))
  (mt:get-lock lock wait-p))
(declaim (notinline acquire-lock))

(defun release-lock (lock)
  (mt:giveup-lock lock))
(declaim (notinline release-lock))

(defmacro with-lock-held ((place) &body body)
  `(mt:with-lock (,place) ,@body))

(defun make-recursive-lock (&optional name)
  (mt:make-lock :name (or name "Anonymous recursive lock") :recursive t))
(declaim (notinline make-recursive-lock))

(defun acquire-recursive-lock (lock &optional (wait-p t))
  (mt:get-lock lock wait-p))
(declaim (notinline acquire-recursive-lock))

(defun release-recursive-lock (lock)
  (mt:giveup-lock lock))
(declaim (notinline release-recursive-lock))

(defmacro with-recursive-lock-held ((place) &body body)
  `(mt:with-lock (,place) ,@body))

;;; Resource contention: condition variables

(defun make-condition-variable (&key name)
  (declare (ignore name))
  (mt:make-condition-variable))
(declaim (notinline make-condition-variable))

(defun condition-wait (condition-variable lock &key timeout)
  (signal-error-if-condition-wait-timeout timeout)
  (mt:condition-wait condition-variable lock)
  t)
(declaim (notinline condition-wait))

(define-condition-wait-compiler-macro)

(defun condition-notify (condition-variable)
  (mt:condition-signal condition-variable))
(declaim (notinline condition-notify))

(defun thread-yield ()
  (mt:thread-yield))
(declaim (notinline thread-yield))

;;; Introspection/debugging

(defun all-threads ()
  (mt:all-threads))
(declaim (notinline all-threads))

(defun interrupt-thread (thread function &rest args)
  (flet ((apply-function ()
           (if args
               (lambda () (apply function args))
               function)))
    (declare (dynamic-extent #'apply-function))
    (mt:interrupt-thread thread (apply-function))))
(declaim (notinline interrupt-thread))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (mt:thread-kill thread))
(declaim (notinline destroy-thread))

(defun thread-alive-p (thread)
  (mt:thread-active-p thread))
(declaim (notinline thread-alive-p))

(defun join-thread (thread)
  (mt:thread-join thread))
(declaim (notinline join-thread))

(mark-supported)
