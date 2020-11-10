(in-package #:net.mfiano.lisp.pyx)

(defstruct (uniform
            (:predicate nil)
            (:copier nil))
  program
  key
  type
  resolved-type
  value
  func)

(defun register-uniform-func (material uniform)
  (let* ((material-spec (spec material))
         (program (shadow:find-program (shader material-spec))))
    (unless (u:href (shadow:uniforms program) (uniform-key uniform))
      (error "Material ~s has the uniform ~s but shader ~s does not use it."
             (name material-spec)
             (uniform-key uniform)
             (shader material-spec)))
    (let* ((type (u:href (shadow:uniforms program) (uniform-key uniform) :type))
           (resolved-type (if (search "SAMPLER" (symbol-name type))
                              :sampler
                              type)))
      (setf (uniform-program uniform) program
            (uniform-type uniform) type
            (uniform-resolved-type uniform) resolved-type
            (uniform-func uniform) (generate-uniform-func material uniform)))))

(defun %generate-uniform-func (material type)
  (let ((func (u:format-symbol :net.mfiano.lisp.shadow "UNIFORM-~a" type)))
    (lambda (k v)
      (funcall func (shader (spec material)) k v))))

(defun generate-uniform-func/sampler (material)
  (lambda (program k v)
    (let ((unit (texture-unit-state material)))
      (incf (texture-unit-state material))
      (bind-texture v unit)
      (shadow:uniform-int program k unit))))

(defun generate-uniform-func/sampler-array (material dimensions)
  (lambda (program k v)
    (loop :with unit-state = (texture-unit-state material)
          :with unit-count = (+ unit-state dimensions)
          :for texture-name :in v
          :for unit :from unit-state :to unit-count
          :do (bind-texture v unit)
          :collect unit :into units
          :finally (incf (texture-unit-state material) dimensions)
                   (shadow:uniform-int-array program k units))))

(defun generate-uniform-func (material uniform)
  (let ((type (uniform-type uniform))
        (resolved-type (uniform-resolved-type uniform)))
    (etypecase type
      (symbol
       (ecase resolved-type
         (:sampler (generate-uniform-func/sampler material))
         (:bool #'shadow:uniform-bool)
         (:int #'shadow:uniform-int)
         (:float #'shadow:uniform-float)
         (:vec2 #'shadow:uniform-vec2)
         (:vec3 #'shadow:uniform-vec3)
         (:vec4 #'shadow:uniform-vec4)
         (:mat2 #'shadow:uniform-mat2)
         (:mat3 #'shadow:uniform-mat3)
         (:mat4 #'shadow:uniform-mat4)))
      (cons
       (destructuring-bind (type . dimensions) type
         (declare (ignore type))
         (ecase resolved-type
           (:sampler (generate-uniform-func/sampler-array material dimensions))
           (:bool #'shadow:uniform-bool-array)
           (:int #'shadow:uniform-int-array)
           (:float #'shadow:uniform-float-array)
           (:vec2 #'shadow:uniform-vec2-array)
           (:vec3 #'shadow:uniform-vec3-array)
           (:vec4 #'shadow:uniform-vec4-array)
           (:mat2 #'shadow:uniform-mat2-array)
           (:mat3 #'shadow:uniform-mat3-array)
           (:mat4 #'shadow:uniform-mat4-array)))))))

(defun as-uniform (func)
  (lambda (entity)
    (declare (ignore entity))
    (funcall func)))

(defun resolve-uniform-value (entity uniform)
  (let ((value (uniform-value uniform)))
    (typecase value
      (boolean value)
      ((or symbol function) (funcall value entity))
      (t value))))

(defun resolve-uniform-func (entity uniform)
  (funcall (uniform-func uniform)
           (uniform-program uniform)
           (uniform-key uniform)
           (resolve-uniform-value entity uniform)))

(defun load-uniform-texture (material uniform)
  (let ((value (uniform-value uniform)))
    (when (eq (uniform-resolved-type uniform) :sampler)
      (let ((material-name (name (spec material)))
            (texture (load-texture value)))
        (setf (uniform-value uniform) texture)
        (pushnew material-name (materials texture))
        (pushnew value (textures material))))))

(defun set-uniforms (entity &rest args)
  (let* ((material (render/current-material entity))
         (uniforms (uniforms material)))
    (u:do-plist (k v args)
      (unless (u:href uniforms k)
        (setf (u:href uniforms k) (make-uniform :key k)))
      (let ((uniform (u:href uniforms k)))
        (setf (uniform-value uniform) v)
        (unless (uniform-func uniform)
          (register-uniform-func material uniform))))))
