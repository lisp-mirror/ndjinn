(in-package #:%pyx.material)

(defun ensure-framebuffer (material)
  (a:when-let* ((spec (spec material))
                (framebuffer-name (spec-framebuffer spec)))
    (a:if-let ((framebuffer (fb:load framebuffer-name)))
      (progn
        (setf (framebuffer material) framebuffer
              (attachments material) (fb:attachment-names->points
                                      framebuffer
                                      (spec-attachments spec)))
        (render:enable-pass (pass spec)))
      (error "Material ~s uses unknown framebuffer ~s."
             (name spec)
             framebuffer-name))))

(defun make-uniforms (material)
  (clrhash (uniforms material))
  (u:do-hash (k v (copy-spec-uniforms (spec material)))
    (let ((uniform (make-uniform :key k :value v)))
      (register-uniform-func material uniform)
      (when (eq (uniform-resolved-type uniform) :sampler)
        (register-uniform-texture material uniform))
      (setf (u:href (uniforms material) k) uniform))))

(defun delete-material-textures (material)
  (u:do-hash-values (v (uniforms material))
    (when (eq (uniform-resolved-type v) :sampler)
      (let ((asset (tex:name (tex:spec (uniform-value v)))))
        (when (asset:find-asset :texture asset)
          (asset:delete-asset :texture asset))))))

(defun make-material (entity name)
  (let* ((spec (find-spec name))
         (material (%make-material :entity entity :spec spec)))
    (make-uniforms material)
    (ensure-framebuffer material)
    (push material (u:href (scene:materials (ctx:current-scene)) name))
    material))

(defun delete (material)
  (let ((name (name (spec material)))
        (framebuffer (framebuffer material))
        (scene-materials (scene:materials (ctx:current-scene))))
    (delete-material-textures material)
    (when framebuffer
      (fb:delete framebuffer))
    (a:deletef (u:href scene-materials name) material)
    (unless (u:href scene-materials name)
      (remhash name scene-materials))))

(live:on-recompile :material data ()
  (let ((shader (shader (find-spec data))))
    (live:recompile :shaders (list shader))
    (dolist (material (u:href (scene:materials (ctx:current-scene)) data))
      (delete-material-textures material)
      (make-uniforms material)
      (a:when-let ((framebuffer (framebuffer material)))
        (fb:delete framebuffer)
        (ensure-framebuffer material)
        (fb::attach-all framebuffer)))))
