(in-package #:pyx)

(defclass material ()
  ((%name :reader name
          :initarg :name)
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms)
   (%funcs :reader funcs
           :initform (u:dict #'eq))
   (%texture-unit-state :accessor texture-unit-state
                        :initform 0)))

(u:define-printer (material stream :type t :identity t)
  (format stream "~s" (name material)))

(defun make-material (spec-name)
  (let ((spec (meta :materials spec-name)))
    (unless spec
      (error "Material ~s not found." spec-name))
    (let* ((uniforms (copy-material-spec-uniforms spec))
           (material (make-instance 'material
                                    :name (name spec)
                                    :shader (shader spec)
                                    :uniforms uniforms)))
      (push material (u:href (materials (database *state*)) spec-name))
      material)))

(defun ensure-material (material/spec)
  (etypecase material/spec
    (material material/spec)
    (symbol (make-material material/spec))))

(defun recompile-material (spec-name)
  (when *state*
    (let ((spec (meta :materials spec-name)))
      (dolist (material (u:href (materials (database *state*)) spec-name))
        (with-slots (%shader %uniforms %funcs) material
          (setf %shader (shader spec)
                %uniforms (copy-material-spec-uniforms spec))
          (u:do-hash-keys (k %uniforms)
            (unless (u:href %funcs k)
              (register-uniform-func material k)))
          (u:do-hash-keys (k %funcs)
            (unless (u:href %uniforms k)
              (remhash k %funcs))))))))
