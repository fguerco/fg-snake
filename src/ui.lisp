;; cl-tui: https://40ants.com/lisp-project-of-the-day/2020/07/0118-cl-tui.html

(in-package :fg-snake)

(defparameter *food-tile* #\$)
(defparameter *head-tile* #\@)
(defparameter *body-tile* #\.)
(defparameter *cage-tile* #\#)

(define-frame main
    (container-frame :split-type :horizontal)
    :on :root)


(define-frame board
    (simple-frame :render 'draw-board)
    :on main)


(define-frame right-bar
    (container-frame :split-type :vertical)
    :on main)


(define-frame stats
    (simple-frame :render 'draw-stats :h 5)
    :on right-bar)

(defun draw-stats (&key frame)
  (let ((y 0))
    (flet ((text (text &rest args)
             (apply #'put-text (list* frame (incf y) 1 text args))))
      (text "Level: ~a" *level*)
      (text "Score: ~a" *score*)
      (text "Snake size: ~a" (length *snake*))
      (text "Steps taken: ~a" *steps*)
      (unless *fail-on-collision*
        (text "No-fail mode")))))


(define-frame log (log-frame) :on right-bar)

(defun add-log (message &rest format-args)
  (apply #'append-line (list* 'log message format-args)))


(defmacro create-ui (&body forms)
  `(with-screen ()
     ,@forms))

(defun draw-cage-line (frame x y size)
  (dotimes (i (1+ size))
    (put-char frame y (+ x i) *cage-tile*)))

(defun draw-cage-column (frame x y size)
  (dotimes (i size)
    (put-char frame (+ y i) x *cage-tile*)))


(defun draw-cage-top-and-bottom (frame offset)
  (draw-cage-line frame (1- offset) (1- offset) (1+ *size-x*))
  (draw-cage-line frame (1- offset) (+ offset *size-y*) (1+ *size-x*)))


(defun draw-cage-sides (frame offset)
  (draw-cage-column frame (1- offset) offset *size-y*)
  (draw-cage-column frame (+ offset *size-x*) offset *size-y*))


(defun draw-cage (frame offset)
  (draw-cage-top-and-bottom frame offset)
  (draw-cage-sides frame offset))


(defun draw-fruit (frame offset)
  (destructuring-bind (x . y) *food*
    (put-char frame (+ offset y) (+ offset x) *food-tile*)))


(defun draw-snake (frame offset)
  (loop for (x . y) in *snake*
        and h = t then nil
        for tile = (if h *head-tile* *body-tile*)
        do (put-char frame
                     (+ offset y) (+ offset x)
                     tile)))


(defun draw-board (&key frame)
  (let ((offset 5))
    (draw-box frame)
    (put-text frame 0 2 " Board ")
    (draw-cage frame offset)
    (draw-fruit frame offset)
    (draw-snake frame offset)))
    
