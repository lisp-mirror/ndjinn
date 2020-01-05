(in-package #:pyx)

(define-component collider/sphere (collider)
  ((%collider/radius :reader collider/radius
                     :initarg :collider/radius
                     :initform 1.0))
  (:sorting :after collider))

(defmethod collide-p ((collider1 collider/sphere) (collider2 collider/sphere))
  (let* ((center1 (transform-point collider1 (collider/center collider1)))
         (center2 (transform-point collider2 (collider/center collider2)))
         (radius1 (transform-vector
                   collider1 (v3:vec (collider/radius collider1) 0f0 0f0)))
         (radius2 (transform-vector
                   collider2 (v3:vec (collider/radius collider2) 0f0 0f0))))
    (<= (v3:distance center1 center2)
        (+ (v3:length radius1)
           (v3:length radius2)))))
