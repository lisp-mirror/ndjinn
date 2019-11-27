(in-package #:pyx)

(define-component render ()
  ((%render/material :reader render/material
                     :initarg :render/material))
  (:sorting :after xform :before sprite))

(defmethod shared-initialize :after ((instance render) slot-names &key)
  (with-slots (%render/material) instance
    (setf %render/material (ensure-material %render/material))
    (u:do-hash-keys (k (uniforms %render/material))
      (register-uniform-func %render/material k))))

(defun render-frame ()
  (map nil #'render-entity (draw-order-entities (database *state*))))

(defun render-entity (entity)
  (with-slots (%shader %uniforms %funcs %texture-unit-state)
      (render/material entity)
    (shadow:with-shader %shader
      (u:do-hash (k v %uniforms)
        (funcall (u:href %funcs k) k v))
      (on-render entity)
      (setf %texture-unit-state 0))))

(defmethod on-render progn ((entity render))
  (a:when-let ((camera (camera *state*)))
    (set-uniforms (render/material entity)
                  :view (camera/view camera)
                  :proj (camera/projection camera))))
