(in-package #:pyx)

(defclass material ()
  ((%id :reader id
        :initarg :id)
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms)
   (%funcs :reader funcs
           :initform (u:dict #'eq))
   (%texture-unit-state :accessor texture-unit-state
                        :initform 0)))

(u:define-printer (material stream :type t :identity t)
  (format stream "~s" (id material)))

(defun make-material (spec-id)
  (let ((spec (meta :materials spec-id)))
    (unless spec
      (error "Material ~s not found." spec-id))
    (let* ((uniforms (copy-material-spec-uniforms spec))
           (material (make-instance 'material
                                    :id (id spec)
                                    :shader (shader spec)
                                    :uniforms uniforms)))
      (push material (u:href (materials *state*) spec-id))
      material)))

(defun update-materials (spec-id)
  (when *state*
    (let ((spec (meta :materials spec-id)))
      (dolist (material (u:href (materials *state*) spec-id))
        (with-slots (%shader %uniforms %funcs) material
          (setf %shader (shader spec)
                %uniforms (copy-material-spec-uniforms spec))
          (u:do-hash-keys (k %uniforms)
            (unless (u:href %funcs k)
              (register-uniform-func material k)))
          (u:do-hash-keys (k %funcs)
            (unless (u:href %uniforms k)
              (remhash k %funcs))))))))
