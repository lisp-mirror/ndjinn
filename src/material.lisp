(in-package #:pyx)

(defclass material ()
  ((%spec :reader spec
          :initarg :spec)
   (%uniforms :reader uniforms
              :initform (u:dict #'eq))
   (%framebuffer :reader framebuffer
                 :initform nil)
   (%attachments :reader attachments
                 :initform nil)
   (%texture-unit-state :accessor texture-unit-state
                        :initform 0)))

(u:define-printer (material stream :type t :identity t)
  (format stream "~s" (name (spec material))))

(defun ensure-material-framebuffer (material)
  (with-slots (%spec %framebuffer %attachments) material
    (a:when-let ((framebuffer-name (framebuffer %spec)))
      (a:if-let ((framebuffer (ensure-framebuffer framebuffer-name)))
        (setf %framebuffer framebuffer
              %attachments (framebuffer-attachment-names->points
                            framebuffer
                            (attachments %spec)))
        (error "Material ~s uses unknown framebuffer ~s."
               (name %spec)
               framebuffer-name)))))

(defun make-material-uniforms (material)
  (clrhash (uniforms material))
  (u:do-hash (k v (copy-material-spec-uniforms (spec material)))
    (let ((uniform (make-instance 'uniform :key k :value v)))
      (register-uniform-func material uniform)
      (setf (u:href (uniforms material) k) uniform))))

(defun make-material (spec-name)
  (a:if-let ((spec (meta :materials spec-name)))
    (let ((material (make-instance 'material :spec spec)))
      (make-material-uniforms material)
      (ensure-material-framebuffer material)
      (push material (u:href (materials (current-scene *state*)) spec-name))
      material)
    (error "Material ~s not found." spec-name)))

(defun register-materials (entity)
  (let ((materials (u:dict #'eq)))
    (dolist (spec-name (render/materials entity))
      (let ((material (make-material spec-name)))
        (setf (u:href materials (pass (spec material))) material)))
    materials))

(defun recompile-material (spec-name)
  (let ((shader (shader (meta :materials spec-name))))
    (recompile-shaders (list shader))
    (dolist (material (u:href (materials (current-scene *state*)) spec-name))
      (make-material-uniforms material)
      (ensure-material-framebuffer material))))
