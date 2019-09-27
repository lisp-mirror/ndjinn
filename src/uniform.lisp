(in-package #:pyx)

(defclass uniforms ()
  ((%specified :reader specified
               :initarg :specified
               :initform (u:dict #'eq))
   (%all :reader all
         :initform (u:dict #'eq))
   (%funcs :reader funcs
           :initform (u:dict #'eq))))

(defun make-uniforms (specified)
  (make-instance 'uniforms :specified specified))

(defun register-uniform-func (entity uniform)
  (with-slots (%render/shader %render/uniforms) entity
    (let* ((program (shadow:find-program %render/shader))
           (type (u:href (shadow:uniforms program) uniform :type))
           (func (generate-uniform-func entity type)))
      (setf (u:href (funcs %render/uniforms) uniform) func))))

(defun %generate-uniform-func (entity type)
  (let ((func (a:format-symbol :shadow "UNIFORM-~a" type)))
    (lambda (k v)
      (funcall func (render/shader entity) k v))))

(defun %generate-uniform-func/sampler (entity)
  (with-slots (%render/shader %render/texture-unit) entity
    (lambda (k v)
      (let ((unit %render/texture-unit)
            (texture-id (id (load-texture v))))
        (incf %render/texture-unit)
        (gl:active-texture unit)
        (bind-texture unit texture-id)
        (shadow:uniform-int %render/shader k unit)))))

(defun %generate-uniform-func/array (entity type)
  (let ((func (a:format-symbol :shadow "UNIFORM-~a-ARRAY" type)))
    (lambda (k v)
      (funcall func (render/shader entity) k v))))

(defun %generate-uniform-func/sampler-array (entity dimensions)
  (with-slots (%render/shader %render/texture-unit) entity
    (lambda (k v)
      (loop :with unit-count = (+ %render/texture-unit dimensions)
            :for texture :in v
            :for texture-id = (id (load-texture texture))
            :for unit :from %render/texture-unit :to unit-count
            :do (gl:active-texture unit)
                (bind-texture unit texture-id)
            :collect unit :into units
            :finally (incf %render/texture-unit dimensions)
                     (shadow:uniform-int-array
                      %render/shader k units)))))

(defun generate-uniform-func (entity type-spec)
  (flet ((resolve-type (type-spec)
           (if (search "SAMPLER" (symbol-name type-spec))
               :sampler
               type-spec)))
    (etypecase type-spec
      (symbol
       (ecase (resolve-type type-spec)
         (:sampler
          (%generate-uniform-func/sampler entity))
         ((:bool :int :float :vec2 :vec3 :vec4 :mat2 :mat3 :mat4)
          (%generate-uniform-func entity type-spec))))
      (cons
       (destructuring-bind (type . dimensions) type-spec
         (ecase (resolve-type type-spec)
           (:sampler
            (%generate-uniform-func/sampler-array entity dimensions))
           ((:bool :int :float :vec2 :vec3 :vec4 :mat2 :mat3 :mat4)
            (%generate-uniform-func/array entity type))))))))

(defun set-uniforms (entity &rest args)
  (let* ((uniforms (render/uniforms entity))
         (all (all uniforms))
         (funcs (funcs uniforms)))
    (u:do-plist (k v args)
      (symbol-macrolet ((func (u:href funcs k)))
        (setf (u:href all k) v)
        (unless func
          (register-uniform-func entity k))
        (funcall func k v)))))
