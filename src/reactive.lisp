(in-package :fg-snake)


(defstruct subject value active subscribers)

(defstruct subscription subject action)


(defmethod print-object ((obj subject) stream)
  (with-slots (value active subscribers) obj
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream ":VALUE ~a :ACTIVE ~a :SUBSCRIBERS ~a"
              value active (length subscribers)))))


(defmethod subscribe ((sub subject) action)
  (let ((ret (make-subscription :subject sub :action action)))
    (with-slots (value active subscribers) sub
      (push ret subscribers)
      (when (or active value)
        (funcall action value)))
    ret))


(defmethod emit ((sub subject) &optional (data t))
  (with-slots (value active subscribers) sub
    (setf active t
          value data)
    (dolist (s (reverse subscribers))
      (funcall (subscription-action s) data))))


(defmacro add-subscription (subject (var &rest filter-values) &body forms)
  (let ((body (if (endp filter-values)
                  `(progn ,@forms)
                  `(if (find ,var (list ,@filter-values) :test #'equalp)
                       (progn ,@forms)))))
    `(subscribe ,subject (lambda (,var) ,body))))
