(in-package #:%pyx.texture)

(defstruct (texture (:constructor %make-texture)
                    (:conc-name nil)
                    (:predicate nil)
                    (:copier nil))
  spec
  target
  id
  width
  height)

(u:define-printer (texture stream)
  (format stream "~s" (name (spec texture))))

(defun calculate-mipmap-levels (spec width height)
  (if (generate-mipmaps spec)
      (loop :for levels = 0 :then (incf levels)
            :while (or (> (ash width (- levels)) 1)
                       (> (ash height (- levels)) 1))
            :finally (return levels))
      1))

(defgeneric update-texture (type texture source))

(defun bind (texture unit)
  (gl:active-texture unit)
  (gl:bind-texture (target texture) (id texture)))

(defun configure (texture)
  (let ((id (id texture))
        (target (target texture))
        (spec (spec texture)))
    (gl:bind-texture target id)
    (when (generate-mipmaps spec)
      (gl:generate-mipmap target))
    (u:do-plist (k v (parameters spec))
      (gl:tex-parameter target k v))
    (gl:bind-texture target 0)))

(defun load-framebuffer-texture (spec width height)
  (img:load nil
            :width width
            :height height
            :pixel-format (pixel-format spec)
            :pixel-type (pixel-type spec)
            :internal-format (internal-format spec)))

(defgeneric load-source (spec type source &key &allow-other-keys)
  (:method :around (spec type source &key)
    (let* ((loaded (call-next-method))
           (source-list (a:flatten (a:ensure-list loaded))))
      (unless (and (every #'img:width source-list)
                   (every #'img:height source-list))
        (error "Texture ~s does not have a width and height set." (name spec)))
      loaded)))

(defun make-target (type)
  (a:format-symbol :keyword "TEXTURE-~a" type))

(defun make-texture (spec type source)
  (let ((texture (%make-texture :spec spec :target (make-target type))))
    (update-texture type texture source)))

(defun load (name &key width height)
  (asset:with-asset-cache :texture name
    (let* ((spec (find-spec name))
           (type (texture-type spec))
           (source (load-source spec type (source spec) :width width :height height))
           (texture (make-texture spec type source)))
      (configure texture)
      texture)))

(live:on-recompile :texture data ()
  (a:when-let ((texture (asset:find-asset :texture data)))
    (gl:delete-texture (id texture))
    (asset:delete-asset :texture data)
    (load data :width (width texture) :height (height texture))))
