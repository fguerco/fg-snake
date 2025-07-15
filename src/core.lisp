(defpackage :fg-snake
  (:use :cl)
  (:use :cl-tui)
  (:export :main))

(in-package :fg-snake)

;;TODO find a smooth curve
(defparameter *diff-table*
  `((10 . .85)
    (20 . .9)
    (30 . .94)
    (,most-positive-fixnum . .97)))

(defparameter *size-x* 20)
(defparameter *size-y* 10)
(defparameter *initial-step-interval* 1000)
(defparameter *difficulty* 0.9)
(defparameter *initial-level* 1)
(defparameter *level* *initial-level*)
(defparameter *direction* nil)
(defparameter *snake* nil)
(defparameter *food* nil)
(defparameter *actions* nil)
(defparameter *steps* 0)
(defparameter *score* 0)
(defparameter *fail-on-collision* t)
(defparameter *welcome-msg*
  '("Welcome! Move with arrow keys or wasd,"
    "Pause with 'p' or Space,"
    "Restart with 'r'"
    "Quit with 'q' or ESC. Have fun!"))


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

;; commands, function, is switch (no value), help message
(defparameter *args*
  '((("-help" "-h") help 1 "show this info")
    (("-level" "-lv") set-level 0 "start with level")
    (("-no-fail") set-no-fail 1 "when a collision happens, lose points intstead of game over")))


(defun set-no-fail ()
  (setf *fail-on-collision* nil))


(defun help ()
  (format t "Command line options:~%")
  (loop for (cmds fn switch info) in *args*
        do (format t "  ~25@<~{~a~^|~}~[ <val>~;~]~>~a~2:*~[ <val>~;~]~%"
                   cmds switch info))
  (uiop:quit 0))


(defun set-level (arg)
  (setf *initial-level* (parse-integer arg)))


(defun find-arg (arg &optional (args *args*))
  (loop for x in args
        if (find arg (car x) :test #'string-equal)
          return x))


(defun load-args (&optional (args (uiop:command-line-arguments)))
  (loop while args
        for (cmds fn switch info) = (find-arg (pop args))
        for next-arg = (when (zerop (or switch 1)) (list (pop args)))
        do (when fn (apply fn next-arg))))


(defun random-item (seq)
  (elt seq (random (length seq))))


(defun unix-time-millis ()
  (multiple-value-bind (sec micro) (sb-ext:get-time-of-day)
    (+ (* sec 1000) (floor micro 1000))))


(defun difficulty (level)
  (cdr (assoc level *diff-table* :test #'<=)))


(defun calculate-step-interval (&optional (level *level*))
  (if (= level 1) *initial-step-interval*
      (* (difficulty level)
         (calculate-step-interval (1- level)))))

