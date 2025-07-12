;; cl-tui: https://40ants.com/lisp-project-of-the-day/2020/07/0118-cl-tui.html

;; TODO implement game over
;; TODO handle more keys
;; TODO add more elements to ui (snake size, time)
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
      (#\escape :quit))))


(defun collect-input ()
  (make-thread
   (lambda ()
     (loop
       for input = (get-input)
       do (setf *input* (append *input* (list input)))
          (add-log (format nil "input: ~a" *input*))))
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

(defun start ()
  (with-screen ()
    (reset)
    (collect-input)
    (loop
      with ellapsed = 0
      for last = 0 then time
      for time = (unix-time-millis)
      for delta = 0 then (- time last)
      until *done*
      for input = (pop *input*)
      do
         (case input
           (:quit (progn (setf *done* t)))
           (:reset (reset))
           ((:north :south :east :west)
            (on-direction-chosen input)
            (setf ellapsed 0))
           (t (if (>= ellapsed *tick*)
                  (progn
                    (move)
                    (setf ellapsed 0))
                  (incf ellapsed delta))))
         (refresh)
         (sleep 1/60))))


(defun main ()
  (handler-case (start)
    (error (c)
      (format t "Error occurred: ~a" c)
      (values 0 c))))
