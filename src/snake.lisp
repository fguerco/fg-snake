;; cl-tui: https://40ants.com/lisp-project-of-the-day/2020/07/0118-cl-tui.html

;; TODO add more elements to ui (snake size, time)
;; TODO implement pause
;; TODO implement game over
;; TODO handle more keys
;; TODO create state to avoid rendering the entire board every time

(in-package :fg-snake)

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
    (setf *tick* (* *tick* *difficulty*))))


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
      (game-over)))


(defun pick-direction ()
  (loop for dir = (car (random-item *moves*))
        until (valid-move-p dir)
        finally (setf *direction* dir)))


(defun reset ()
  (new-snake)
  (pick-direction)
  (spawn-food))


(defun draw-board (&key frame)
  (destructuring-bind (fx . fy) *food*
    (put-char frame fy fx #\$))
  (loop for (x . y) in *snake*
        and h = t then nil
        do (put-char frame y x (if h #\@ #\#))))


(defun on-direction-chosen (direction)
  (add-log (format nil "new direction chosen: ~a" direction))
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
    do (queue-action action)
       (add-log (format nil "actions: ~a" *actions*))))


(defun collect-input-thread ()
  (sb-thread:make-thread 'collect-input
                         :name "collect-input-thread"))

(defun game-over ()
  (setf *done* t))


(define-frame main
    (container-frame :split-type :horizontal)
    :on :root)

(define-frame board
    (simple-frame :render 'draw-board)
    :on main
    :w *size-x*)

(define-frame log (log-frame) :on main)

(defun add-log (message)
  (append-line 'log message))

(defun game-loop ()
  (loop
      with ellapsed = 0 and paused = nil and done = nil
      for last = 0 then time
      for time = (unix-time-millis)
      for delta = 0 then (- time last)
      until done
      for action = (pop-action)
      do
         (case action
           (:quit (setf done t))
           (:reset (reset))
           (:pause (setf paused (not paused)))
           ((:north :south :east :west)
            (unless paused
              (on-direction-chosen action)
              (setf ellapsed 0)))
           (t
            (unless paused
              (if (>= ellapsed *tick*)
                  (progn
                    (move)
                    (setf ellapsed 0))
                  (incf ellapsed delta)))))
         (refresh)
         (sleep 1/60)))

(defun start ()
  (with-screen ()
    (reset)
    (collect-input-thread)
    (game-loop)))


(defun main ()
  (handler-case (start)
    (error (c)
      (format t "Error occurred: ~a" c)
      (values 0 c))))
