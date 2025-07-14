
;; TODO implement game over
;; TODO clean up code

(in-package :fg-snake)

(defparameter *input-mutex* (sb-thread:make-mutex :name "input"))


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
          (spawn-food)
          (increase-speed)))
      (wrong-move)))


(defun pick-direction ()
  (loop for dir = (car (random-item *moves*))
        until (valid-move-p dir)
        finally (setf *direction* dir)))


(defun reset ()
  (setf *level* *initial-level*)
  (setf *steps* 0)
  (new-snake)
  (pick-direction)
  (spawn-food))


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
  (loop
    for action = (get-input)
    do (queue-action action)))


(defun collect-input-thread ()
  (sb-thread:make-thread #'collect-input
                         :name "collect-input-thread"))

(defun wrong-move ()
  (add-log "Ouch! Can't go this way :'("))


(defmacro pause (var)
  `(progn
     (setf ,var (not ,var))
     (add-log (if ,var "Paused" "Resumed"))))

(defun game-loop ()
  (mapc #'add-log *welcome-msg*)
  (loop
    with ellapsed = 0 and paused = nil
    for last = 0 then time
    for time = (unix-time-millis)
    for delta = 0 then (- time last)
    for action = (pop-action)
    do
       (case action
         (:quit (return))
         (:reset (reset))
         (:pause (pause paused))
         ((:north :south :east :west)
          (unless paused
            (on-direction-chosen action)
            (incf *steps*)
            (setf ellapsed 0)))
         (t
          (unless paused
            (if (>= ellapsed (calculate-step-interval))
                (progn
                  (move)
                  (incf *steps*)
                  (setf ellapsed 0))
                (incf ellapsed delta)))))
       (refresh)
       (sleep 1/60)))


(defun start ()
  (reset)
  (create-ui
    (collect-input-thread)
    (game-loop)))


(defun main ()
  (load-args)
  (start))
