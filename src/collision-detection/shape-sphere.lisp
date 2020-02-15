(in-package #:%pyx.collision-detection)

(defclass shape/sphere (shape)
  ((%radius :reader radius
            :initform 1)))

(defmethod initialize-instance :after ((instance shape/sphere) &key)
  (with-slots (%entity %radius) instance
    (v3:with-components ((s (c/transform:get-scale %entity)))
      (unless (= sx sy sz)
        (error "Sphere shape for entity ~s can only have a uniform scale."
               %entity))
      (setf %radius sx))))
