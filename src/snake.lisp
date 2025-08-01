
;; TODO clean up code

(in-package :fg-snake)

(defparameter *input-mutex* (sb-thread:make-mutex :name "input"))

(define-subscription *food-eaten* (next)
  (push next *snake*)
  (spawn-food)
  (increase-speed))


(define-subscription *collision-happened* (fail)
  (if *fail-on-collision*
      (game-over)
      (wrong-move)))


;; TODO rethink about growing the snake here
(defun food-eaten (next)
  (emit *food-eaten* next))


(defun game-over ()
  (add-log "Game over. Press 'r' to start a new game")
  (setf *game-over* t))


(defun queue-action (action &optional (mutex *input-mutex*))
  (sb-thread:with-mutex (mutex)
    (push-to-back action *actions*)))


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


(defun tail-to-head (x y)
  (setf *snake* (cons (cons x  y) (butlast *snake*))))


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
    (unless (or (and at-east-edge (= x 1))
                (and at-west-edge (= x -1))
                (and at-north-edge (= y -1))
                (and at-south-edge (= y 1))
                (snake-collision-p x y))
      direction)))


(defun move (&optional (direction *direction*))
  (if (valid-move-p direction)
      (let* ((next (next-position direction))
             (x (car next))
             (y (cdr next))
             (grow (equalp next *food*)))
        (if grow
            (food-eaten next)
            (if neck
                (tail-to-head x y)
                (move-head x y))))
      (emit *collision-happened* *fail-on-collision*)))


(defun pick-direction ()
  (loop for dir = (car (random-item *moves*))
        until (valid-move-p dir)
        finally (setf *direction* dir)))


(defun reset ()
  (setf *level* *initial-level*
        *steps* 0
        *ellapsed* 0
        *score* 0
        *game-paused* nil
        *game-over* nil)
  (new-snake)
  (pick-direction)
  (spawn-food)
  (add-log "New Game started"))


(defun advance (&optional (direction *direction* direction-supplied-p))
  (when direction-supplied-p
    (setf *direction* direction))
  (move)
  (incf *steps*)
  (setf *ellapsed* 0))


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


(defun toggle-pause ()
  (toggle *game-paused*)
  (add-log "Game ~:[Resumed~;Paused~]" *game-paused*))


(defun game-running-p ()
  (not (or *game-paused* *game-over*)))


(defun process-action (action delta)
  (case action
    (:quit action)
    (:reset (reset))
    (:pause (unless *game-over* (toggle-pause)))
    ((:north :south :east :west)
     (when (game-running-p)
       (advance action)))
    (t
     (when (game-running-p)
       (if (>= *ellapsed* (calculate-step-interval))
           (advance)
           (incf *ellapsed* delta))))))


(defun game-loop ()
  (loop
    for last = 0 then time
    for time = (floor (get-internal-real-time) 1000)
    for delta = 0 then (- time last)
    for result = (process-action (pop-action) delta)
    do (when (eq result :quit) (return))
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
