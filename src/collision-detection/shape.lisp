(in-package #:net.mfiano.lisp.pyx)

(defclass collider-shape ()
  ((%type :reader shape-type
          :initarg :type)
   (%entity :reader entity
            :initarg :entity)
   (%center :reader center
            :initarg :center
            :initform (v3:vec))))

(defun make-collider-shape (entity shape-spec)
  (destructuring-bind (type . args) (u:ensure-list shape-spec)
    (let ((class (u:format-symbol :net.mfiano.lisp.pyx
                                  "COLLIDER-SHAPE/~a"
                                  type)))
      (apply #'make-instance class :type type :entity entity args))))

(defgeneric update-collider-shape (shape)
  (:method (shape)))
