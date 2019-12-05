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
  (clear-screen)
  (map nil #'render-entity (draw-order-entities (database *state*))))

(defun render-entity (entity)
  (with-slots (%spec %framebuffer %output %uniforms %funcs %texture-unit-state)
      (render/material entity)
    (with-framebuffer %framebuffer (:output %output)
      (shadow:with-shader (shader %spec)
        (u:do-hash (k v %uniforms)
          (funcall (u:href %funcs k) k v))
        (on-render entity)
        (setf %texture-unit-state 0)))))

(defmethod on-render :around ((entity render))
  (let ((material (render/material entity)))
    (with-slots (%features/enabled %features/disabled %blend-mode %depth-mode)
        (spec material)
      (apply #'gl:enable %features/enabled)
      (apply #'gl:disable %features/disabled)
      (apply #'gl:blend-func %blend-mode)
      (gl:depth-func %depth-mode)
      (a:when-let ((camera (camera *state*)))
        (set-uniforms material
                      :view (camera/view camera)
                      :proj (camera/projection camera)))
      (call-next-method)
      (apply #'gl:enable %features/disabled)
      (apply #'gl:disable %features/enabled)
      (apply #'gl:blend-func +gl-blend-mode+)
      (gl:depth-func +gl-depth-mode+))))
