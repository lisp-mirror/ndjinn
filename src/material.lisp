(in-package #:pyx)

(defclass material ()
  ((%spec :reader spec
          :initarg :spec)
   (%uniforms :reader uniforms
              :initarg :uniforms)
   (%framebuffer :reader framebuffer
                 :initform nil)
   (%attachment-points :reader attachment-points
                       :initform nil)
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
  (with-slots (%spec %framebuffer %attachment-points) material
    (let ((framebuffer-name (framebuffer %spec)))
      (if framebuffer-name
          (progn
            (setf %framebuffer (ensure-framebuffer framebuffer-name)
                  %attachment-points (framebuffer-attachment-names->points
                                      %framebuffer
                                      (attachments %spec)))
            (unless %framebuffer
              (error "Material ~s uses unknown framebuffer ~s."
                     (name %spec)
                     framebuffer-name)))
          (setf %framebuffer nil
                %attachment-points nil)))))

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
          (unless (nth-value 1 (u:href %funcs k))
            (register-uniform-func material k)))
        (u:do-hash-keys (k %funcs)
          (unless (nth-value 1 (u:href %uniforms k))
            (remhash k %funcs)))))))
