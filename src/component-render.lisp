(in-package #:pyx)

(define-component render (:after xform :before sprite)
  (:shader nil
   :texture-unit 0))

(defun set-sampler (entity key value)
  (with-slots (%render/texture-unit %render/shader) entity
    (let ((unit %render/texture-unit))
      (incf %render/texture-unit)
      (bind-texture unit value)
      (shadow:uniform-int %render/shader key unit))))

(u:eval-always
  (defun get-uniform-func (type)
    (ecase type
      (:bool 'shadow:uniform-bool)
      (:int 'shadow:uniform-int)
      (:float 'shadow:uniform-float)
      (:vec2 'shadow:uniform-vec2)
      (:vec3 'shadow:uniform-vec3)
      (:vec4 'shadow:uniform-vec4)
      (:mat2 'shadow:uniform-mat2)
      (:mat3 'shadow:uniform-mat3)
      (:mat4 'shadow:uniform-mat4))))

(defmacro with-render (shader &body (uniforms . body))
  (a:with-gensyms (program-id program)
    `(unwind-protect
          (let* ((,program ,shader)
                 (,program-id (shadow:get-program-id ,program)))
            (gl:use-program ,program-id)
            ,@(loop :for (type key value) :in uniforms
                    :for func = (get-uniform-func type)
                    :collect `(,func ,program ,key ,value))
            ,@body)
       (gl:use-program 0))))

(defmethod on-render progn ((entity render))
  (let ((camera (camera *state*)))
    (with-render (render/shader entity)
      ((:mat4 :view (camera/view camera))
       (:mat4 :proj (camera/projection camera))))))
