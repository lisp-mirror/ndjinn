(in-package #:pyx)

(define-component render (:after xform :before sprite)
  (:shader nil
   :texture-unit 0
   :uniforms nil))

(defmacro uniforms (&body body)
  `(list ,@(loop :for form :in body :collect `(list ,@form))))

(defun set-sampler (entity key value)
  (with-slots (%render/texture-unit %render/shader) entity
    (let ((unit %render/texture-unit)
          (texture-id (id (load-texture value))))
      (incf %render/texture-unit)
      (bind-texture unit texture-id)
      (shadow:uniform-int %render/shader key unit))))

(defun set-uniform (entity program uniform)
  (declare (ignorable entity))
  (destructuring-bind (type key value) uniform
    (ecase type
      (:sampler (set-sampler entity key value))
      (:bool (shadow:uniform-bool program key value))
      (:int (shadow:uniform-int program key value))
      (:float (shadow:uniform-float program key value))
      (:vec2 (shadow:uniform-vec2 program key value))
      (:vec3 (shadow:uniform-vec3 program key value))
      (:vec4 (shadow:uniform-vec4 program key value))
      (:mat2 (shadow:uniform-mat2 program key value))
      (:mat3 (shadow:uniform-mat3 program key value))
      (:mat4 (shadow:uniform-mat4 program key value)))))

(defmacro with-render (entity &body (uniforms . body))
  (a:with-gensyms (program-id program)
    `(unwind-protect
          (let* ((,program (render/shader ,entity))
                 (,program-id (shadow:get-program-id ,program)))
            (gl:use-program ,program-id)
            ,@(loop :for uniform :in uniforms
                    :collect `(set-uniform ,entity ,program (list ,@uniform)))
            (dolist (x (render/uniforms ,entity))
              (set-uniform ,entity ,program x))
            ,@body)
       (setf (render/texture-unit ,entity) 0)
       (gl:use-program 0))))

(defmethod on-render progn ((entity render))
  (a:when-let ((camera (camera *state*)))
    (with-render entity
      ((:mat4 :view (camera/view camera))
       (:mat4 :proj (camera/projection camera))))))
