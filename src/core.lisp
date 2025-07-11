(defpackage :fg-snake
  (:use :cl)
  (:use :cl-tui)
  (:use :bt2)
  (:export :run))

(in-package :fg-snake)

(defparameter *size-x* 10)
(defparameter *size-y* 10)
(defparameter *tick* 1.0)
(defparameter *difficulty* 0.85)
(defparameter *direction* nil)
(defparameter *snake* nil)
(defparameter *food* nil)


(define-symbol-macro head (car *snake*))
(define-symbol-macro neck (cadr *snake*))

(define-symbol-macro at-north-edge (zerop (cdr head)))
(define-symbol-macro at-south-edge (= (1- *size-y*) (cdr head)))
(define-symbol-macro at-west-edge (zerop (car head)))
(define-symbol-macro at-east-edge (= (1- *size-x*) (car head)))


(defmacro start-thread (thread name &body forms)
  `(setf ,thread
         (make-thread
          (lambda () ,@forms)
          :name ,name)))

(defmacro stop-thread (thread)
  `(when (and (threadp ,thread) (thread-alive-p ,thread))
     (destroy-thread ,thread)
     (setf ,thread nil)))


(defmacro define-worker-thread ((name) &body forms)
  (let ((stop-name (intern (format nil "~a-STOP" name))))
    `(let (thread)
       (defun ,name ()
         (start-thread thread ,(format nil "thread-~a" name)
           ,@forms))
       (defun ,stop-name () (stop-thread thread)))))

(defun random-item (seq)
  (elt seq (random (length seq))))
