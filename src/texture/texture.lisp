(in-package #:net.mfiano.lisp.pyx)

(defclass texture ()
  ((%spec :reader spec
          :initarg :spec)
   (%target :reader target
            :initarg :target)
   (%id :accessor id)
   (%width :accessor width)
   (%height :accessor height)
   (%materials :accessor materials
               :initform nil)))

(u:define-printer (texture stream)
  (format stream "~s" (name (spec texture))))

(defun calculate-texture-mipmap-levels (spec width height)
  (if (generate-mipmaps spec)
      (loop :for levels = 0 :then (incf levels)
            :while (or (> (ash width (- levels)) 1)
                       (> (ash height (- levels)) 1))
            :finally (return levels))
      1))

(defgeneric update-texture (type texture source))

(defun bind-texture (texture unit)
  (gl:active-texture unit)
  (gl:bind-texture (target texture) (id texture)))

(defun configure-texture (texture)
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
  (load-image nil
              :width width
              :height height
              :pixel-format (pixel-format spec)
              :pixel-type (pixel-type spec)
              :internal-format (internal-format spec)))

(defgeneric load-texture-source (spec type source &key &allow-other-keys)
  (:method :around (spec type source &key)
    (let* ((loaded (call-next-method))
           (source-list (u:flatten (u:ensure-list loaded))))
      (unless (and (every #'width source-list)
                   (every #'height source-list))
        (error "Texture ~s does not have a width and height set." (name spec)))
      loaded)))

(defun make-texture-target (type)
  (u:format-symbol :keyword "TEXTURE-~a" type))

(defun make-texture (spec type source)
  (let ((texture (make-instance 'texture
                                :spec spec
                                :target (make-texture-target type))))
    (update-texture type texture source)
    texture))

(defun load-texture (name &key width height)
  (with-asset-cache :texture name
    (log:debug :pyx "Loading texture: ~s..." name)
    (let* ((spec (find-texture-spec name))
           (type (texture-type spec))
           (source (load-texture-source spec
                                        type
                                        (source spec)
                                        :width width
                                        :height height))
           (texture (make-texture spec type source)))
      (configure-texture texture)
      (log:debug :pyx "Texture loaded: ~s" name)
      texture)))

(on-recompile :texture data ()
  (u:when-let ((texture (find-asset :texture data)))
    (gl:delete-texture (id texture))
    (delete-asset :texture data)
    (load-texture data :width (width texture) :height (height texture))
    (dolist (material-name (materials texture))
      (recompile :material material-name))
    (log:debug :pyx "Recompiled texture: ~s" data)))
