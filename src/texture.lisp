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
  (let* ((id (gl:create-texture :texture-2d))
         (texture (make-instance
                   'texture
                   :spec spec
                   :type :texture-2d
                   :id id
                   :width (width source)
                   :height (height source))))
    (%gl:texture-storage-2d id
                            1
                            (internal-format source)
                            (width source)
                            (height source))
    (when (data source)
      (gl/texture-sub-image-2d
       id
       0
       0
       0
       (width source)
       (height source)
       (pixel-format source)
       (pixel-type source)
       (data source)))
    texture))

(defmethod make-texture (spec (source list))
  (let* ((id (gl:create-texture :texture-2d-array))
         (layer0 (first source))
         (texture (make-instance
                   'texture
                   :spec spec
                   :type :texture-2d-array
                   :id id
                   :width (width layer0)
                   :height (height layer0))))
    (%gl:texture-storage-3d id
                            1
                            (internal-format layer0)
                            (width layer0)
                            (height layer0)
                            (length source))
    (loop :for image :in source
          :for layer :from 0
          :do (gl/texture-sub-image-3d id
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
    texture))

(defun bind-texture (texture unit)
  (%gl:bind-texture-unit unit (id texture)))

(defun configure-texture (texture)
  (with-slots (%spec %id) texture
    (when (generate-mipmaps-p %spec)
      (gl:generate-texture-mipmap %id))
    (u:do-hash (k v (parameters %spec))
      (gl:texture-parameter %id k v))))

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
      (resource-lookup 'texture texture-name
        (let* ((spec (find-texture-spec texture-name))
               (source (load-texture-source spec))
               (texture (make-texture spec source)))
          (configure-texture texture)
          texture))
    (error ()
      (load-texture 'default))))

(defun load-framebuffer-texture (framebuffer attachment texture-name)
  (with-slots (%name %point %width %height) attachment
    (let ((cached-name (a:symbolicate (name framebuffer) '#:/ %name)))
      (resource-lookup 'texture cached-name
        (let* ((spec (find-texture-spec texture-name))
               (source (load-texture-source spec
                                            :width (funcall %width)
                                            :height (funcall %height)))
               (texture (make-texture spec source)))
          (configure-texture texture)
          texture)))))

(defun recompile-texture (spec-name)
  (a:when-let ((texture (u:href (resources *state*) 'texture spec-name)))
    (gl:delete-texture (id texture))
    (remhash spec-name (u:href (resources *state*) 'texture))
    (load-texture spec-name)))
