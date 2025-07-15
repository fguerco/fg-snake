
;; TODO implement game over
;; TODO clean up code

(in-package :fg-snake)

(defparameter *ellapsed* 0)
(defparameter *game-paused* nil)
(defparameter *game-over* nil)
(defparameter *input-mutex* (sb-thread:make-mutex :name "input"))
(defparameter *collision* (make-subject))
(defparameter *food-eaten* (make-subject))
(defparameter *player-action* (make-subject))
(defparameter *snake-moved* (make-subject))

(add-subscription *collision* (x)
  (if *fail-on-collision*
      (game-over)
      (wrong-move)))

(add-subscription *food-eaten* (x)
  (spawn-food)
  (increase-speed))

(defun game-over ()
  (add-log "Game over. Press 'r' to start a new game")
  (setf *game-over* t))

(defun queue-action (action &optional (mutex *input-mutex*))
  (sb-thread:with-mutex (mutex)
    (setf *actions* (append *actions* (list action)))))


(defun pop-action (&optional (mutex *input-mutex*))
  (sb-thread:with-mutex (mutex)
    (pop *actions*)))


(defun generate-place ()
  (let ((x (random *size-x*))
        (y (random *size-y*)))
    (cons x y)))

(defun new-snake ()
  (setf *snake* (list (generate-place))))


(defun spawn-food ()
  (loop for food = (generate-place)
        while (find food *snake* :test #'equalp)
        finally (return (setf *food* food))))


(defun increase-speed ()
  (when (zerop (mod (length *snake*) 5))
    (add-log "Level ~a!" (incf *level*))))


(defun copy-head ()
  (copy-list head))


(defun direction-data (direction)
  (cdr (assoc direction *moves*)))


(defun move-head (x y)
  (setf (car head) x)
  (setf (cdr head) y))


(defun tail-to-head ()
  (setf *snake* (cons (copy-head) (butlast *snake*))))


(defun next-position (direction)
  (destructuring-bind (x . y) (direction-data direction)
    (cons (+ (car head) x)
          (+ (cdr head) y))))


(defun snake-collision-p (x y)
  (let ((new-head (cons (+ (car head) x)
                        (+ (cdr head) y))))
    (find new-head *snake* :test #'equalp)))


(defun valid-move-p (direction)
  (destructuring-bind (x . y) (direction-data direction)
    (not (or (and at-east-edge (= x 1))
             (and at-west-edge (= x -1))
             (and at-north-edge (= y -1))
             (and at-south-edge (= y 1))
             (snake-collision-p x y)))))


(defun move (&optional (direction *direction*))
  (if (valid-move-p direction)
      (let* ((next (next-position direction))
             (x (car next))
             (y (cdr next))
             (grow (equalp next *food*)))
        (if grow
            (push next *snake*)
            (when neck (tail-to-head)))
        (move-head x y)
        (when grow
          (emit *food-eaten*)))
      (emit *collision*)))


(defun pick-direction ()
  (loop for dir = (car (random-item *moves*))
        until (valid-move-p dir)
        finally (setf *direction* dir)))


(defun reset ()
  (setf *level* *initial-level*
        *steps* 0
        *ellapsed* 0
        *game-paused* nil
        *game-over* nil)
  (new-snake)
  (pick-direction)
  (spawn-food)
  (add-log "New Game started"))


(defun on-direction-chosen (direction)
  (move (setf *direction* direction)))


(defun get-input ()
  (let ((key (read-key)))
    (case key
      ((:key-up #\w #\8) :north)
      ((:key-down #\s #\2) :south)
      ((:key-right #\d #\6) :east)
      ((:key-left #\a #\4) :west)
      (#\r :reset)
      ((#\p #\space) :pause)
      ((#\q #\escape) :quit))))


(defun collect-input ()
  (loop for action = (get-input)
        do (queue-action action)))


(defun collect-input-thread ()
  (sb-thread:make-thread #'collect-input
                         :name "collect-input-thread"))

(defun wrong-move ()
  (add-log "Ouch! Can't go this way :'("))


(defmacro toggle-pause (var)
  `(progn
     (setf ,var (not ,var))
     (add-log (if ,var "Paused" "Resumed"))))

(defun game-running-p ()
    (not (or *game-paused* *game-over*)))

(defun game-loop ()
  (loop
    for last = 0 then time
    for time = (unix-time-millis)
    for delta = 0 then (- time last)
    for action = (pop-action)
    do
       (case action
         (:quit (return))
         (:reset (reset))
         (:pause (unless *game-over* (toggle-pause *game-paused*)))
         ((:north :south :east :west)
          (when (game-running-p)
            (on-direction-chosen action)
            (incf *steps*)
            (setf *ellapsed* 0)))
         (t
          (when (game-running-p)
            (if (>= *ellapsed* (calculate-step-interval))
                (progn
                  (move)
                  (incf *steps*)
                  (setf *ellapsed* 0))
                (incf *ellapsed* delta)))))
       (refresh)
       (sleep 1/60)))


(defun start ()
  (create-ui
    (mapc #'add-log *welcome-msg*)
    (reset)
    (collect-input-thread)
    (game-loop)))


(defun main ()
  (load-args)
  (start))
