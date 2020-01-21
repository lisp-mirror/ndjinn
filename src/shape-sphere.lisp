(in-package #:pyx)

(defclass shape/sphere (shape)
  ((%radius :reader radius
            :initform 1f0)))

(defmethod initialize-instance :after ((instance shape/sphere) &key)
  (with-slots (%entity) instance
    (v3:with-components ((s (current (xform/scaling %entity))))
      (unless (= sx sy sz)
        (error "Sphere shape for entity ~s can only have a uniform scale."
               %entity))
      (setf (slot-value instance '%radius) sx))))
