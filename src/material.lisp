(in-package #:%pyx.material)

(defstruct (material (:constructor %make-material)
                     (:conc-name nil)
                     (:predicate nil)
                     (:copier nil))
  spec
  entity
  (uniforms (u:dict #'eq))
  framebuffer
  attachments
  (texture-unit-state 0))

(u:define-printer (material stream :type t :identity t)
  (format stream "~s" (name (spec material))))

(defun ensure-framebuffer (material)
  (a:when-let* ((spec (spec material))
                (framebuffer-name (spec-framebuffer spec)))
    (a:if-let ((framebuffer (fb:load framebuffer-name)))
      (setf (framebuffer material) framebuffer
            (attachments material) (fb:attachment-names->points
                                    framebuffer
                                    (spec-attachments spec)))
      (error "Material ~s uses unknown framebuffer ~s."
             (name spec)
             framebuffer-name))))

(defun make-uniforms (material)
  (clrhash (uniforms material))
  (u:do-hash (k v (copy-spec-uniforms (spec material)))
    (let ((uniform (make-uniform :key k :value v)))
      (register-uniform-func material uniform)
      (load-uniform-texture uniform)
      (setf (u:href (uniforms material) k) uniform))))

(defun make-material (entity name)
  (let* ((spec (find-spec name))
         (material (%make-material :entity entity :spec spec)))
    (make-uniforms material)
    (ensure-framebuffer material)
    (push material (u:href (scene:materials (ctx:current-scene)) name))
    material))

(util::on-recompile :material data ()
  (let ((shader (shader (find-spec data))))
    (util::recompile :shaders (list shader))
    (dolist (material (u:href (scene:materials (ctx:current-scene)) data))
      (make-uniforms material)
      (ensure-framebuffer material))))
