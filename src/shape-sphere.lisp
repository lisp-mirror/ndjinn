(in-package #:pyx)

(defclass shape/sphere (shape)
  ((%radius :reader radius
            :initform 1f0)))
