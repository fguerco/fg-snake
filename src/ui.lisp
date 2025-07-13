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
  (put-text frame 1 1 "Level: ~a" *level*)
  (put-text frame 2 1 "Snake size: ~a" (length *snake*))
  (put-text frame 3 1 "Steps taken: ~a" *steps*))


(define-frame log (log-frame) :on right-bar)

(defun add-log (message &rest format-args)
  (apply #'append-line (list* 'log message format-args)))


(defmacro create-ui (&body forms)
  `(with-screen ()
     ,@forms))


(defun draw-cage-top-and-bottom (frame offset)
  (loop for y in (list (1- offset) (+ offset *size-y*))
        do (loop for x from (1- offset) upto (+ offset *size-x*)
                 do (put-char frame y x *cage-tile*))))


(defun draw-cage-sides (frame offset)
  (loop for y from offset upto (+ offset *size-y*)
        do (loop for x in (list (1- offset) (+ offset *size-x*))
                 do (put-char frame y x *cage-tile*))))


(defun draw-cage (frame offset)
  (draw-cage-top-and-bottom frame offset)
  (draw-cage-sides frame offset))


(defun draw-fruit (frame offset)
  (destructuring-bind (x . y) *food*
    (put-char frame (+ offset y) (+ offset x) #\$)))


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
    
