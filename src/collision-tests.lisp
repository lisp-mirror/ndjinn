(in-package #:pyx)

(defgeneric collide-p (collider1 collider2)
  (:method (collider1 collider2)))

(defmethod collide-p ((collider1 collider/sphere) (collider2 collider/sphere))
  (<= (v3:distance (transform-point collider1 (collider/center collider1))
                   (transform-point collider2 (collider/center collider2)))
      (+ (v3:length (transform-vector collider1 (v3:vec 1f0 0f0 0f0)))
         (v3:length (transform-vector collider2 (v3:vec 1f0 0f0 0f0))))))
