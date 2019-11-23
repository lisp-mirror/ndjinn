(in-package #:pyx)

(defun register-uniform-func (material uniform)
  (with-slots (%name %shader %uniforms %funcs) material
    (let ((program (shadow:find-program %shader)))
      (unless (u:href (shadow:uniforms program) uniform)
        (error "Material ~s has the uniform ~s but shader ~s does not use it."
               %name uniform %shader))
      (let* ((type (u:href (shadow:uniforms program) uniform :type))
             (func (generate-uniform-func material type)))
        (setf (u:href %funcs uniform) func)))))

(defun %generate-uniform-func (material type)
  (let ((func (a:format-symbol :shadow "UNIFORM-~a" type)))
    (lambda (k v)
      (funcall func (shader material) k v))))

(defun %generate-uniform-func/sampler (material)
  (with-slots (%shader %texture-unit-state) material
    (lambda (k v)
      (let ((unit %texture-unit-state)
            (texture (load-texture v)))
        (incf %texture-unit-state)
        (gl:active-texture unit)
        (bind-texture texture unit)
        (shadow:uniform-int %shader k unit)))))

(defun %generate-uniform-func/array (material type)
  (let ((func (a:format-symbol :shadow "UNIFORM-~a-ARRAY" type)))
    (lambda (k v)
      (funcall func (shader material) k v))))

(defun %generate-uniform-func/sampler-array (material dimensions)
  (with-slots (%shader %texture-unit-state) material
    (lambda (k v)
      (loop :with unit-count = (+ %texture-unit-state dimensions)
            :for texture-name :in v
            :for texture = (load-texture texture-name)
            :for unit :from %texture-unit-state :to unit-count
            :do (gl:active-texture unit)
                (bind-texture texture unit)
            :collect unit :into units
            :finally (incf %texture-unit-state dimensions)
                     (shadow:uniform-int-array %shader k units)))))

(defun generate-uniform-func (material type-spec)
  (flet ((resolve-type (type-spec)
           (if (search "SAMPLER" (symbol-name type-spec))
               :sampler
               type-spec)))
    (etypecase type-spec
      (symbol
       (ecase (resolve-type type-spec)
         (:sampler
          (%generate-uniform-func/sampler material))
         ((:bool :int :float :vec2 :vec3 :vec4 :mat2 :mat3 :mat4)
          (%generate-uniform-func material type-spec))))
      (cons
       (destructuring-bind (type . dimensions) type-spec
         (ecase (resolve-type type-spec)
           (:sampler
            (%generate-uniform-func/sampler-array material dimensions))
           ((:bool :int :float :vec2 :vec3 :vec4 :mat2 :mat3 :mat4)
            (%generate-uniform-func/array material type))))))))

(defun set-uniforms (material &rest args)
  (let ((funcs (funcs material)))
    (u:do-plist (k v args)
      (symbol-macrolet ((func (u:href funcs k)))
        (unless func
          (register-uniform-func material k))
        (funcall func k v)))))
