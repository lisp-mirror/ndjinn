(in-package #:pyx)

(define-component render (:after xform :before sprite)
  (:shader nil
   :texture-unit 0
   :uniforms (u:dict #'eq)))

(defmethod shared-initialize :after ((instance render) slot-names &key)
  (with-slots (%render/uniforms) instance
    (setf %render/uniforms (make-uniforms %render/uniforms))
    (u:do-hash-keys (k (specified %render/uniforms))
      (register-uniform-func instance k))))

(defun render (entity)
  (with-slots (%render/shader %render/uniforms %render/texture-unit) entity
    (when (has-component-p 'render entity)
      (shadow:with-shader %render/shader
        (u:do-hash (k v (specified %render/uniforms))
          (funcall (u:href (funcs %render/uniforms) k) k v))
        (on-render entity)
        (setf %render/texture-unit 0)))))

(defmethod on-render progn ((entity render))
  (a:when-let ((camera (camera *state*)))
    (set-uniforms entity
                  :view (camera/view camera)
                  :proj (camera/projection camera))))
