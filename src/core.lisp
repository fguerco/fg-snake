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
(defparameter *ellapsed* 0)
(defparameter *game-paused* nil)
(defparameter *game-over* nil)

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

(defparameter *food-eaten* (make-subject))
(defparameter *collision-happened* (make-subject))


(define-symbol-macro head (car *snake*))
(define-symbol-macro neck (cadr *snake*))

(define-symbol-macro at-north-edge (zerop (cdr head)))
(define-symbol-macro at-south-edge (= (1- *size-y*) (cdr head)))
(define-symbol-macro at-west-edge (zerop (car head)))
(define-symbol-macro at-east-edge (= (1- *size-x*) (car head)))


(defun random-item (seq)
  (elt seq (random (length seq))))


(defun difficulty (level)
  (cdr (assoc level *diff-table* :test #'<=)))


(defun calculate-step-interval (&optional
                                  (level *level*)
                                  (step-interval *initial-step-interval*))
  (if (= level 1) step-interval
      (* (difficulty level)
         (calculate-step-interval (1- level) step-interval))))

(defmacro toggle (var)
  `(setf ,var (not ,var)))

(defmacro push-to-back (obj list)
  `(setf ,list (nconc ,list (list ,obj))))


;; Score adjustments - thinking about moving this out of here

(defun score-points (&optional (level *level*))
  (* 5 (ceiling level 3)))

(define-subscription *food-eaten* (next)
  (incf *score* (score-points)))


(define-subscription *collision-happened* (fail)
  (unless fail
    (when (minusp (decf *score* (score-points)))
      (setf *score* 0))))
