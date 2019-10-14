(in-package #:pyx)

(defclass texture ()
  ((%spec :reader spec
          :initarg :spec)
   (%type :reader texture-type
          :initarg :type)
   (%id :reader id
        :initarg :id)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)))

(defgeneric make-texture (spec source))

(defmethod make-texture (spec (source image))
  (let* ((id (gl:gen-texture))
         (texture (make-instance
                   'texture
                   :spec spec
                   :type :texture-2d
                   :id id
                   :width (width source)
                   :height (height source))))
    (gl:bind-texture :texture-2d id)
    (gl:tex-image-2d :texture-2d
                     0
                     (internal-format source)
                     (width source)
                     (height source)
                     0
                     (pixel-format source)
                     (pixel-type source)
                     (data source))
    (gl:bind-texture :texture-2d 0)
    texture))

(defmethod make-texture (spec (source list))
  (let* ((id (gl:gen-texture))
         (layer0 (first source))
         (texture (make-instance
                   'texture
                   :spec spec
                   :type :texture-2d-array
                   :id id
                   :width (width layer0)
                   :height (height layer0))))
    (gl:bind-texture :texture-2d-array id)
    (%gl:tex-storage-3d :texture-2d-array
                        1
                        (internal-format layer0)
                        (width layer0)
                        (height layer0)
                        (length source))
    (loop :for image :in source
          :for layer :from 0
          :do (gl:tex-sub-image-3d :texture-2d-array
                                   0
                                   0
                                   0
                                   layer
                                   (width image)
                                   (height image)
                                   1
                                   (pixel-format image)
                                   (pixel-type image)
                                   (data image)))
    (gl:bind-texture :texture-2d-array 0)
    texture))

(defun bind-texture (texture unit)
  (with-slots (%type %id) texture
    (gl:active-texture unit)
    (gl:bind-texture %type %id)))

(defun configure-texture (texture)
  (with-slots (%spec %type %id) texture
    (gl:bind-texture %type %id)
    (when (generate-mipmaps-p %spec)
      (gl:generate-mipmap %type))
    (u:do-hash (k v (parameters %spec))
      (gl:texture-parameter %id k v))
    (gl:bind-texture %type 0)))

(defun load-texture-source (spec &key width height)
  (etypecase (source spec)
    (null (read-image nil
                      :width width
                      :height height
                      :pixel-format (pixel-format spec)
                      :pixel-type (pixel-type spec)
                      :internal-format (internal-format spec)))
    (string (read-image (source spec)))
    (list (mapcar #'read-image (source spec)))))

(defun load-texture (texture-name)
  (handler-case
      (cache-lookup :texture texture-name
        (let* ((spec (find-texture-spec texture-name))
               (source (load-texture-source spec))
               (texture (make-texture spec source)))
          (configure-texture texture)
          texture))
    (error ()
      (load-texture 'default))))

(defun load-framebuffer-texture (framebuffer attachment)
  (with-slots (%name %point %width %height) attachment
    (let ((spec-name (a:format-symbol :pyx "FRAMEBUFFER-~a" (car %point)))
          (texture-name (a:format-symbol :pyx "FRAMEBUFFER-~a-~a"
                                         (name framebuffer)
                                         %name)))
      (cache-lookup :texture texture-name
        (let* ((spec (find-texture-spec spec-name))
               (source (load-texture-source spec
                                            :width (funcall %width)
                                            :height (funcall %height)))
               (texture (make-texture spec source)))
          (configure-texture texture)
          texture)))))
