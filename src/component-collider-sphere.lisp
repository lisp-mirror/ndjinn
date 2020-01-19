(in-package #:pyx)

(define-component collider/sphere (collider)
  ((%collider/radius :reader collider/radius
                     :initarg :collider/radius
                     :initform 1f0))
  (:sorting :after render))

(defun scale-collider/sphere (entity)
  (v3:with-components ((s (current (xform/scaling entity))))
    (unless (= sx sy sz)
      (error "Colliders must have a uniform scale."))
    (v3:scale! s s (collider/radius entity))))

(defun initialize-collider-visualization/sphere (entity)
  (when (collider/visualize entity)
    (when (or (has-component-p entity 'mesh)
              (has-component-p entity 'render))
      (error "Entity ~s has a collider to be visualized, but it must not have ~
              a mesh or render component attached." entity))
    (attach-component entity 'mesh
                      :mesh/file "collider-sphere.glb"
                      :mesh/name "sphere")
    (attach-component entity 'render
                      :render/materials '(collider/mesh))))

(defmethod collide-p ((collider1 collider/sphere) (collider2 collider/sphere))
  (<= (v3:distance (transform-point collider1 (collider/center collider1))
                   (transform-point collider2 (collider/center collider2)))
      (+ (v3:length (transform-vector collider1 (v3:vec 1f0 0f0 0f0)))
         (v3:length (transform-vector collider2 (v3:vec 1f0 0f0 0f0))))))

;;; component protocol

(define-hook :attach (entity collider/sphere)
  (scale-collider/sphere entity)
  (initialize-collider-visualization/sphere entity)
  (register-collider entity))

(define-hook :detach (entity collider)
  (deregister-collider entity)
  (setf collider/target nil))
