(in-package #:pyx)

(defclass shape ()
  ((%entity :reader entity
            :initarg :entity)
   (%center :reader center
            :initarg :center
            :initform (v3:vec))))

(defun make-shape (entity shape-spec)
  (destructuring-bind (type . args) (a:ensure-list shape-spec)
    (let ((class (a:format-symbol :pyx "SHAPE/~a" type)))
      (apply #'make-instance class :entity entity args))))

(defgeneric update-shape (shape)
  (:method (shape)))
