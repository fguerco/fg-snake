(in-package :fg-snake)

;; commands, function, is switch (no value), help message
(defparameter *args*
  '((("-help" "-h") :help 1 "show this info")
    (("-size-x" "-width" "-W") :size-x 0  "set horizontal board size")
    (("-size-y" "-height" "-H") :size-y 0 "set vertical board size")
    (("-level" "-lv") :level 0 "set starting level")
    (("-no-fail") :no-fail 1 "when a collision happens, lose points intstead of game over")))

(defun help ()
  (format t "Command line options:~%")
  (loop for (cmds fn switch info) in *args*
        do (format t "  ~27@<~{~a~^|~}~[ <val>~;~]~>~a~2:*~[ to <val>~;~]~%"
                   cmds switch info))
  (uiop:quit 0))


(defun set-arg (arg &optional value)
  (case arg
    (:help (help))
    (:no-fail (setf *fail-on-collision* nil))
    (:size-x (setf *size-x* (parse-integer value)))
    (:size-y (setf *size-y* (parse-integer value)))
    (:level (setf *initial-level* (parse-integer value)))))


(defun find-arg (arg &optional (args *args*))
  (loop for x in args
        if (find arg (car x) :test #'equal)
          return x))

(defun load-args (&optional (args (uiop:command-line-arguments)))
  (loop while args
        for (cmds key switch) = (find-arg (pop args))
        for arg-value = (when (zerop (or switch 1)) (pop args))
        do (format t "key: ~a, value: ~a~%" key arg-value)
        when key
          do (set-arg key arg-value)))
