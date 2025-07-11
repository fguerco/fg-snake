;; cl-tui: https://40ants.com/lisp-project-of-the-day/2020/07/0118-cl-tui.html

;; TODO implement game over
;; TODO handle more keys
;; TODO add more elements to ui (snake size, time)
;; TODO create state to avoid rendering the entire board every time

(in-package :fg-snake)

;; directions with delta movement values
(defparameter *moves*
  '((:north 0 . -1)
    (:south 0 . 1)
    (:east 1 . 0)
    (:west -1 . 0)))

(defun generate-place ()
  (let ((x (random *size-x*))
        (y (random *size-y*)))
    (cons x y)))


(defun spawn-food ()
  (loop for food = (generate-place)
        while (find food *snake* :test #'equalp)
        finally (return (setf *food* food))))

(defun increase-speed ()
  (when (zerop (mod (length *snake*) 5))
    (setf *tick* (* *tick* 0.85))))

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


(defun move (&optional (direction *direction*))
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
  *snake*)


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


(defun random-item (seq)
  (elt seq (random (length seq))))


(defun pick-direction ()
  (loop for dir = (car (random-item *moves*))
        until (valid-move-p dir)
        finally (return dir)))


(defun reset ()
  (setf *snake* (list (generate-place)))
  (setf *direction* (pick-direction))
  (spawn-food))


(defun draw-board (&key frame)
  (destructuring-bind (fx . fy) *food*
    (put-char frame fy fx #\$))
  (loop for (x . y) in *snake*
        and h = t then nil
        do (put-char frame y x (if h #\@ #\#))))


(defun on-direction-chosen (direction)
  (auto-walk-stop)
  (setf *direction* direction)
  (refresh)
  (auto-walk))


(defun get-input ()
  (let ((key (read-key)))
    (case key
      ((:key-up #\w #\8) :north)
      ((:key-down #\s #\2) :south)
      ((:key-right #\d #\6) :east)
      ((:key-left #\a #\4) :west))))



(define-worker-thread (auto-walk)
  (loop
    (move *direction*)
    (refresh)
    (sleep *tick*)))

(define-worker-thread (collect-input)
  (loop
    for input = (get-input)
    do (case input
         (:quit (return))
         (:refresh (reset))
         ((:north :south :east :west)
          (on-direction-chosen input)))))

(defun game-over ()
  (auto-walk-stop))

(define-frame board
    (simple-frame :render 'draw-board)
    :on :root
    :w *size-x*
    :h *size-y*)


(defun game-run ()
  (reset)
  (with-screen ()
    (refresh)
    (collect-input)
    (auto-walk)
    (loop)))


(defun run ()
  (handler-case (game-run)
    (error (c)
      (format t "Error occurred: ~a" c)
      (values 0 c))))
