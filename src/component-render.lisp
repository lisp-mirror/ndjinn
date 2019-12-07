(in-package #:pyx)

(define-component render ()
  ((%render/materials :reader render/materials
                      :initarg :render/materials)
   (%render/order :reader render/order
                  :initarg :render/order
                  :initform :default)
   (%render/current-material :accessor render/current-material
                             :initform nil)
   (%render/passes :accessor render/passes
                   :initform nil))
  (:sorting :after xform :before sprite))

(defmethod shared-initialize :after ((instance render) slot-names &key)
  (with-slots (%render/materials) instance
    (setf %render/materials (register-materials instance))))

(defmethod on-component-removed (entity (type (eql 'render)))
  (deregister-draw-order entity))

(defun clear-render-pass (pipeline pass)
  (let ((options (u:href (pass-options pipeline) pass)))
    (destructuring-bind (&key clear-color clear-buffers) options
      (v4:with-components ((v clear-color))
        (gl:clear-color vx vy vz vw)
        (apply #'gl:clear clear-buffers)))))

(defun render-frame ()
  (let ((scene-spec (spec (current-scene *state*))))
    (map nil #'render-pass (pass-order (pipeline scene-spec)))))

(defun render-pass (pass)
  (a:when-let* ((scene (current-scene *state*))
                (order (u:href (draw-order scene) pass)))
    (clear-render-pass (pipeline (spec scene)) pass)
    (loop :for (entity . material) :in order
          :do (setf (render/current-material entity) material)
              (render-entity entity))))

(defun render-entity (entity)
  (with-slots (%spec %framebuffer %output %uniforms %funcs %texture-unit-state)
      (render/current-material entity)
    (with-framebuffer %framebuffer (:output %output)
      (shadow:with-shader (shader %spec)
        (u:do-hash (k v %uniforms)
          (funcall (u:href %funcs k) k v))
        (on-render entity)
        (setf %texture-unit-state 0)))))

(defmethod on-render :around ((entity render))
  (let ((material (render/current-material entity)))
    (with-slots (%features/enabled %features/disabled %blend-mode %depth-mode)
        (spec material)
      (apply #'gl:enable %features/enabled)
      (apply #'gl:disable %features/disabled)
      (apply #'gl:blend-func %blend-mode)
      (gl:depth-func %depth-mode)
      (a:when-let ((camera (camera (current-scene *state*))))
        (set-uniforms material
                      :view (camera/view camera)
                      :proj (camera/projection camera)))
      (call-next-method)
      (apply #'gl:enable %features/disabled)
      (apply #'gl:disable %features/enabled)
      (apply #'gl:blend-func +gl-blend-mode+)
      (gl:depth-func +gl-depth-mode+))))
