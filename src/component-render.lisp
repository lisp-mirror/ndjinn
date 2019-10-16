(in-package #:pyx)

(define-component render (:after xform :before sprite)
  (:material nil))

(defmethod shared-initialize :after ((instance render) slot-names &key)
  (with-slots (%render/material) instance
    (unless %render/material
      (error "Entity ~s does not have a material specified." instance))
    (setf %render/material (make-material %render/material))
    (u:do-hash-keys (k (uniforms %render/material))
      (register-uniform-func %render/material k))))

(defun render (entity)
  (resolve-model entity)
  (when (has-component-p 'render entity)
    (with-slots (%shader %uniforms %funcs %texture-unit-state)
        (render/material entity)
      (shadow:with-shader %shader
        (u:do-hash (k v %uniforms)
          (funcall (u:href %funcs k) k v))
        (on-render entity)
        (setf %texture-unit-state 0)))))

(defmethod on-render progn ((entity render))
  (a:when-let ((camera (camera *state*)))
    (set-uniforms (render/material entity)
                  :view (camera/view camera)
                  :proj (camera/projection camera))))
