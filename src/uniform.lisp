(in-package #:pyx)

(defclass uniform ()
  ((%key :reader key
         :initarg :key)
   (%type :accessor uniform-type
          :initform nil)
   (%resolved-type :accessor resolved-type
                   :initform nil)
   (%value :accessor value
           :initarg :value)
   (%func :accessor func
          :initform nil)))

(defun register-uniform-func (material uniform)
  (with-slots (%name %shader) (spec material)
    (let ((program (shadow:find-program %shader)))
      (unless (u:href (shadow:uniforms program) (key uniform))
        (error "Material ~s has the uniform ~s but shader ~s does not use it."
               %name uniform %shader))
      (let* ((type (u:href (shadow:uniforms program) (key uniform) :type))
             (resolved-type (if (search "SAMPLER" (symbol-name type))
                                :sampler
                                type)))
        (setf (uniform-type uniform) type
              (resolved-type uniform) resolved-type
              (func uniform) (generate-uniform-func material uniform))))))

(defun %generate-uniform-func (material type)
  (let ((func (a:format-symbol :shadow "UNIFORM-~a" type)))
    (lambda (k v)
      (funcall func (shader (spec material)) k v))))

(defun %generate-uniform-func/sampler (material)
  (with-slots (%spec %texture-unit-state) material
    (lambda (k v)
      (let ((unit %texture-unit-state)
            (texture (load-texture v)))
        (incf %texture-unit-state)
        (bind-texture texture unit)
        (shadow:uniform-int (shader %spec) k unit)))))

(defun %generate-uniform-func/array (material type)
  (let ((func (a:format-symbol :shadow "UNIFORM-~a-ARRAY" type)))
    (lambda (k v)
      (funcall func (shader (spec material)) k v))))

(defun %generate-uniform-func/sampler-array (material dimensions)
  (with-slots (%spec %texture-unit-state) material
    (lambda (k v)
      (loop :with unit-count = (+ %texture-unit-state dimensions)
            :for texture-name :in v
            :for texture = (load-texture texture-name)
            :for unit :from %texture-unit-state :to unit-count
            :do (bind-texture texture unit)
            :collect unit :into units
            :finally (incf %texture-unit-state dimensions)
                     (shadow:uniform-int-array (shader %spec) k units)))))

(defun generate-uniform-func (material uniform)
  (let ((type (uniform-type uniform))
        (resolved-type (resolved-type uniform)))
    (etypecase type
      (symbol
       (ecase resolved-type
         (:sampler
          (%generate-uniform-func/sampler material))
         ((:bool :int :float :vec2 :vec3 :vec4 :mat2 :mat3 :mat4)
          (%generate-uniform-func material type))))
      (cons
       (destructuring-bind (type . dimensions) type
         (ecase resolved-type
           (:sampler
            (%generate-uniform-func/sampler-array material dimensions))
           ((:bool :int :float :vec2 :vec3 :vec4 :mat2 :mat3 :mat4)
            (%generate-uniform-func/array material type))))))))

(defun resolve-uniform-value/sampler (uniform)
  (let ((value (value uniform)))
    (if (typep value 'symbol)
        value
        (error "Sampler uniform ~s must be a symbol denoting a texture."
               (key uniform)))))

(defun resolve-uniform-value (uniform)
  (let ((value (value uniform)))
    (typecase value
      (boolean value)
      ((or symbol function)
       (funcall value))
      (t value))))

(defun resolve-uniform-func (uniform)
  (funcall (func uniform)
           (key uniform)
           (case (resolved-type uniform)
             (:sampler (resolve-uniform-value/sampler uniform))
             (t (resolve-uniform-value uniform)))))

(defun set-uniforms (entity &rest args)
  (let* ((material (render/current-material entity))
         (uniforms (uniforms material)))
    (u:do-plist (k v args)
      (unless (u:href uniforms k)
        (setf (u:href uniforms k) (make-instance 'uniform :key k)))
      (let ((uniform (u:href uniforms k)))
        (setf (value uniform) v)
        (unless (func uniform)
          (register-uniform-func material uniform))))))
