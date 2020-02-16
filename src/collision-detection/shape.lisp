(in-package #:pyx)

(defclass collider-shape ()
  ((%entity :reader entity
            :initarg :entity)
   (%center :reader center
            :initarg :center
            :initform (v3:vec))))

(defun make-collider-shape (entity shape-spec)
  (destructuring-bind (type . args) (a:ensure-list shape-spec)
    (let ((class (a:format-symbol :pyx "COLLIDER-SHAPE/~a" type)))
      (apply #'make-instance class :entity entity args))))

(defgeneric update-collider-shape (shape)
  (:method (shape)))
