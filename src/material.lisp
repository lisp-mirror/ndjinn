(in-package #:pyx)

(defclass material ()
  ((%spec :reader spec
          :initarg :spec)
   (%uniforms :reader uniforms
              :initarg :uniforms)
   (%framebuffer :reader framebuffer
                 :initarg :framebuffer
                 :initform nil)
   (%output :reader output
            :initarg :output)
   (%funcs :reader funcs
           :initform (u:dict #'eq))
   (%texture-unit-state :accessor texture-unit-state
                        :initform 0)))

(u:define-printer (material stream :type t :identity t)
  (format stream "~s" (name (spec material))))

(defun register-materials (entity)
  (let (materials)
    (dolist (spec (render/materials entity))
      (let ((material (make-material spec)))
        (u:do-hash-keys (k (uniforms material))
          (register-uniform-func material k))
        (push material materials)))
    materials))

(defun ensure-material-framebuffer (material)
  (with-slots (%spec %framebuffer %output) material
    (destructuring-bind (&optional framebuffer-name attachment-names)
        (output %spec)
      (when framebuffer-name
        (setf %framebuffer (ensure-framebuffer framebuffer-name)
              %output (framebuffer-attachment-names->points
                       %framebuffer
                       attachment-names))
        (unless %framebuffer
          (error "Material ~s uses unknown framebuffer ~s."
                 (name %spec)
                 framebuffer-name))))))

(defun make-material (spec-name)
  (a:if-let ((spec (meta :materials spec-name)))
    (u:mvlet* ((uniforms (copy-material-spec-uniforms spec))
               (material (make-instance 'material
                                        :spec spec
                                        :uniforms uniforms)))
      (ensure-material-framebuffer material)
      (push material (u:href (materials (current-scene *state*)) spec-name))
      material)
    (error "Material ~s not found." spec-name)))

(defun recompile-material (spec-name)
  (let ((spec (meta :materials spec-name)))
    (dolist (material (u:href (materials (current-scene *state*)) spec-name))
      (with-slots (%uniforms %funcs) material
        (setf %uniforms (copy-material-spec-uniforms spec))
        (ensure-material-framebuffer material)
        (u:do-hash-keys (k %uniforms)
          (unless (u:href %funcs k)
            (register-uniform-func material k)))
        (u:do-hash-keys (k %funcs)
          (unless (u:href %uniforms k)
            (remhash k %funcs)))))))
