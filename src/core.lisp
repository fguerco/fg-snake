(defpackage :fg-snake
  (:use :cl)
  (:use :cl-tui)
  (:export :main))

(in-package :fg-snake)

(defparameter *size-x* 10)
(defparameter *size-y* 10)
(defparameter *tick* 1000)
(defparameter *difficulty* 0.85)
(defparameter *direction* nil)
(defparameter *snake* nil)
(defparameter *food* nil)
(defparameter *actions* nil)

;; directions with delta movement values
(defparameter *moves*
  '((:north 0 . -1)
    (:south 0 . 1)
    (:east 1 . 0)
    (:west -1 . 0)))


(define-symbol-macro head (car *snake*))
(define-symbol-macro neck (cadr *snake*))

(define-symbol-macro at-north-edge (zerop (cdr head)))
(define-symbol-macro at-south-edge (= (1- *size-y*) (cdr head)))
(define-symbol-macro at-west-edge (zerop (car head)))
(define-symbol-macro at-east-edge (= (1- *size-x*) (car head)))

(defun queue-action (action)
  (setf *actions* (append *actions* (list action))))

(defun pop-action ()
  (pop *actions*))

(defun random-item (seq)
  (elt seq (random (length seq))))

(defun unix-time-millis ()
  (multiple-value-bind (sec micro) (sb-ext:get-time-of-day)
    (+ (* sec 1000) (floor micro 1000))))
