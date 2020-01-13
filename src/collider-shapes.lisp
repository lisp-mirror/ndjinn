(in-package #:pyx)

(defclass collider-shape ()
  ((%collider :reader collider
              :initarg :collider)))

(defclass sphere (collider-shape)
  ((%radius :reader radius
            :initarg :radius
            :initform 1f0)))

(defmethod initialize-instance :after ((instance sphere) &key)
  (let ((collider (collider instance)))
    (when (collider/visualize collider)
      (v3:with-components ((s (current (xform/scaling collider))))
        (unless (= sx sy sz)
          (error "Sphere colliders must have a uniform scale."))
        (v3:scale! s s (radius instance)))
      (attach-component collider 'mesh
                        :mesh/file "collider-sphere.glb"
                        :mesh/name "sphere"))))

;;; internal collider protocol

(defmethod collide-p :filter :shape ((collider1 sphere) (collider2 sphere))
  (<= (v3:distance (transform-point collider1 (collider/center collider1))
                   (transform-point collider2 (collider/center collider2)))
      (+ (v3:length (transform-vector collider1 (v3:vec 1f0 0f0 0f0)))
         (v3:length (transform-vector collider2 (v3:vec 1f0 0f0 0f0))))))
