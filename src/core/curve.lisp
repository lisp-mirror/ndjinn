(in-package #:net.mfiano.lisp.pyx)

;;; spec

(defstruct (curve-spec
            (:constructor %make-curve-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (points nil :type list))

(defun update-curve-spec (name points)
  (let ((spec (u:href =meta/curves= name)))
    (setf (curve-spec-points spec) points)))

(defun make-curve-spec (name points)
  (let ((spec (%make-curve-spec :name name)))
    (setf (u:href =meta/curves= name) spec)
    (update-curve-spec name points)
    spec))

(defmacro define-curve (name options &body body)
  (declare (ignore options))
  (let ((points `(list ,@(mapcar (lambda (x) `(v3:vec ,@x)) body))))
    `(progn
       (unless (curve:point-count-valid-p (length ,points))
         (error "Point count invalid for curve ~a." ',name))
       (if (u:href =meta/curves= ',name)
           (update-curve-spec ',name ,points)
           (make-curve-spec ',name ,points)))))
