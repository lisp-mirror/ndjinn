(in-package #:%pyx.texture)

(defstruct (texture (:constructor %make-texture)
                    (:conc-name nil)
                    (:predicate nil)
                    (:copier nil))
  spec
  texture-type
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

(defgeneric make-texture (spec source))

(defmethod make-texture (spec (source res.img:image))
  (let* ((id (gl:create-texture :texture-2d))
         (width (res.img:width source))
         (height (res.img:height source))
         (texture (%make-texture :spec spec
                                 :texture-type :texture-2d
                                 :id id
                                 :width width
                                 :height height)))
    (gl:texture-storage-2d id
                           (calculate-mipmap-levels spec width height)
                           (res.img:internal-format source)
                           width
                           height)
    (a:when-let ((data (res.img:data source)))
      (gl:texture-sub-image-2d id
                               0
                               0
                               0
                               width
                               height
                               (res.img:pixel-format source)
                               (res.img:pixel-type source)
                               data))
    texture))

(defmethod make-texture (spec (source list))
  (let* ((id (gl:create-texture :texture-2d-array))
         (layer0 (first source))
         (width (res.img:width layer0))
         (height (res.img:height layer0))
         (texture (%make-texture :spec spec
                                 :texture-type :texture-2d-array
                                 :id id
                                 :width width
                                 :height height)))
    (gl:texture-storage-3d id
                           (calculate-mipmap-levels spec width height)
                           (res.img:internal-format layer0)
                           width
                           height
                           (length source))
    (loop :for image :in source
          :for layer :from 0
          :do (gl:texture-sub-image-3d id
                                       0
                                       0
                                       0
                                       layer
                                       (res.img:width image)
                                       (res.img:height image)
                                       1
                                       (res.img:pixel-format image)
                                       (res.img:pixel-type image)
                                       (res.img:data image)))
    texture))

(defun bind (texture unit)
  (gl:bind-texture-unit unit (id texture)))

(defun configure (texture)
  (let ((id (id texture))
        (spec (spec texture)))
    (when (generate-mipmaps spec)
      (gl:generate-texture-mipmap id))
    (u:do-plist (k v (parameters spec))
      (gl:texture-parameter id k v))))

(defgeneric load-source-image (spec source &key &allow-other-keys))

(defmethod load-source-image (spec (source null) &key width height)
  (res.img:load nil
                :width width
                :height height
                :pixel-format (pixel-format spec)
                :pixel-type (pixel-type spec)
                :internal-format (internal-format spec)))

(defmethod load-source-image (spec (source string) &key)
  (res.img:load source))

(defmethod load-source-image (spec (source list) &key)
  (mapcar #'res.img:load source))

(defun load (name &key width height)
  (res:with-resource-cache :texture name
    (let* ((spec (find-spec name))
           (source (load-source-image spec
                                      (source spec)
                                      :width width
                                      :height height))
           (texture (make-texture spec source)))
      (configure texture)
      texture)))

(live:on-recompile :texture data ()
  (a:when-let ((texture (res:find-resource :texture data)))
    (gl:delete-texture (id texture))
    (res:delete-resource :texture data)
    (load data :width (width texture) :height (height texture))))
