(in-package #:ndjinn)

(defstruct (texture
            (:constructor %make-texture)
            (:predicate nil)
            (:copier nil))
  (spec nil :type (or texture-spec null))
  (target :texture-2d :type keyword)
  (id 0 :type u:ub16)
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (materials nil :type list))

(u:define-printer (texture stream)
  (format stream "~s" (texture-spec-name (texture-spec texture))))

(defun calculate-texture-mipmap-levels (spec width height)
  (if (texture-spec-generate-mipmaps spec)
      (loop :for levels = 0 :then (incf levels)
            :while (or (> (ash width (- levels)) 1)
                       (> (ash height (- levels)) 1))
            :finally (return levels))
      1))

(defgeneric update-texture (type texture source))

(defun bind-texture (texture unit)
  (gl:active-texture unit)
  (gl:bind-texture (texture-target texture) (texture-id texture)))

(defun configure-texture (texture)
  (let ((id (texture-id texture))
        (target (texture-target texture))
        (spec (texture-spec texture)))
    (gl:bind-texture target id)
    (when (texture-spec-generate-mipmaps spec)
      (gl:generate-mipmap target))
    (u:do-plist (k v (texture-spec-parameters spec))
      (gl:tex-parameter target k v))
    (gl:bind-texture target 0)))

(defun load-framebuffer-texture (spec width height)
  (load-image nil
              :width width
              :height height
              :pixel-format (texture-spec-pixel-format spec)
              :pixel-type (texture-spec-pixel-type spec)
              :internal-format (texture-spec-internal-format spec)))

(defgeneric load-texture-source (spec type source &key &allow-other-keys)
  (:method :around (spec type source &key)
    (let* ((loaded (call-next-method))
           (source-list (u:flatten (u:ensure-list loaded))))
      (unless (and (every #'image-width source-list)
                   (every #'image-height source-list))
        (error "Texture ~s does not have a width and height set."
               (texture-spec-name spec)))
      loaded)))

(defun make-texture-target (type)
  (u:format-symbol :keyword "TEXTURE-~a" type))

(defun make-texture (spec type source)
  (let ((texture (%make-texture :spec spec
                                :target (make-texture-target type))))
    (update-texture type texture source)
    texture))

(defun load-texture (name &key width height)
  (with-asset-cache :texture name
      (log:debug :ndjinn "Loading texture: ~s..." name)
    (let* ((spec (find-texture-spec name))
           (type (texture-spec-type spec))
           (source (load-texture-source spec
                                        type
                                        (texture-spec-source spec)
                                        :width width
                                        :height height))
           (texture (make-texture spec type source)))
      (configure-texture texture)
      (log:debug :ndjinn "Texture loaded: ~s" name)
      texture)))

(on-recompile :texture data ()
  (u:when-let ((texture (find-asset :texture data)))
    (gl:delete-texture (texture-id texture))
    (delete-asset :texture data)
    (load-texture data
                  :width (texture-width texture)
                  :height (texture-height texture))
    (dolist (material-name (texture-materials texture))
      (recompile :material material-name))
    (log:debug :ndjinn "Recompiled texture: ~s" data)))
