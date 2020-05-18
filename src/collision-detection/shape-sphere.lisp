(in-package #:net.mfiano.lisp.pyx)

(defclass collider-shape/sphere (collider-shape)
  ((%radius :reader radius
            :initform 1)))

(defmethod initialize-instance :after ((instance collider-shape/sphere) &key)
  (with-slots (%entity %radius) instance
    (v3:with-components ((s (get-scale %entity)))
      (unless (= sx sy sz)
        (error "Sphere shape for entity ~s can only have a uniform scale."
               %entity))
      (setf %radius sx))))
