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

(u:define-printer (texture stream)
  (format stream "~s" (name (spec texture))))

(defun calculate-mipmap-levels (width height)
  (loop :for levels = 0 :then (incf levels)
        :while (or (> (ash width (- levels)) 1)
                   (> (ash height (- levels)) 1))
        :finally (return levels)))

(defgeneric make-texture (spec source))

(defmethod make-texture (spec (source image))
  (let* ((id (gl:create-texture :texture-2d))
         (width (width source))
         (height (height source))
         (texture (make-instance
                   'texture
                   :spec spec
                   :type :texture-2d
                   :id id
                   :width width
                   :height height)))
    (%gl:texture-storage-2d id
                            (calculate-mipmap-levels width height)
                            (internal-format source)
                            width
                            height)
    (when (data source)
      (gl/texture-sub-image-2d
       id
       0
       0
       0
       width
       height
       (pixel-format source)
       (pixel-type source)
       (data source)))
    texture))

(defmethod make-texture (spec (source list))
  (let* ((id (gl:create-texture :texture-2d-array))
         (layer0 (first source))
         (width (width layer0))
         (height (height layer0))
         (texture (make-instance
                   'texture
                   :spec spec
                   :type :texture-2d-array
                   :id id
                   :width width
                   :height height)))
    (%gl:texture-storage-3d id
                            (calculate-mipmap-levels width height)
                            (internal-format layer0)
                            width
                            height
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

(defun load-texture (texture-name &key width height)
  (resource-lookup 'texture texture-name
    (let* ((spec (find-texture-spec texture-name))
           (source (load-texture-source
                    spec
                    :width width
                    :height height))
           (texture (make-texture spec source)))
      (configure-texture texture)
      texture)))

(defun recompile-texture (spec-name)
  (a:when-let ((texture (u:href (resources *state*) 'texture spec-name)))
    (gl:delete-texture (id texture))
    (remhash spec-name (u:href (resources *state*) 'texture))
    (load-texture spec-name)))
