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

(defgeneric make-texture (spec type source))

(defmethod make-texture (spec (type (eql :2d)) source)
  (let* ((id (gl:create-texture :texture-2d))
         (width (res.img:width source))
         (height (res.img:height source))
         (texture (%make-texture :spec spec
                                 :target type
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

(defmethod make-texture (spec (type (eql :2d-array)) source)
  (let* ((id (gl:create-texture :texture-2d-array))
         (layer0 (first source))
         (width (res.img:width layer0))
         (height (res.img:height layer0))
         (texture (%make-texture :spec spec
                                 :target type
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

(defmethod make-texture (spec (type (eql :cube-map)) source)
  (let* ((id (gl:create-texture :texture-cube-map))
         (layer0 (first source))
         (width (res.img:width layer0))
         (height (res.img:height layer0))
         (texture (%make-texture :spec spec
                                 :target type
                                 :id id
                                 :width width
                                 :height height)))
    (gl:texture-storage-2d id
                           (calculate-mipmap-levels spec width height)
                           (res.img:internal-format layer0)
                           width
                           height)
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

(defun load-framebuffer-texture (spec width height)
  (res.img:load nil
                :width width
                :height height
                :pixel-format (pixel-format spec)
                :pixel-type (pixel-type spec)
                :internal-format (internal-format spec)))

(defgeneric load-source (spec type &key &allow-other-keys)
  (:method :around (spec type &key)
    (let* ((source (call-next-method))
           (source-list (a:ensure-list source)))
      (unless (and (every #'res.img:width source-list)
                   (every #'res.img:height source-list))
        (error "Texture ~s does not have a width and height set." (name spec)))
      source)))

(defmethod load-source (spec (type (eql :2d)) &key width height)
  (typecase (source spec)
    ((or null (integer 1 1))
     (load-framebuffer-texture spec width height))
    (string
     (res.img:load (source spec)))
    (t (error "Unsupported source for 2D texture: ~s." (name spec)))))

(defmethod load-source (spec (type (eql :2d-array)) &key width height)
  (let ((source (source spec)))
    (cond
      ((null source)
       (list (load-framebuffer-texture spec width height)))
      ((typep source '(integer 1))
       (loop :repeat (source spec)
             :collect (load-framebuffer-texture spec width height)))
      ((and (typep source 'a:proper-list)
            (every #'stringp source))
       (mapcar #'res.img:load source))
      (t (error "Unsupported source for 2D array texture: ~s." (name spec))))))

(defmethod load-source (spec (type (eql :cube-map)) &key width height)
  (let ((source (source spec))
        (valid-keys '(:x+ :x- :y+ :y- :z+ :z-)))
    (cond
      ((typep source '(or null (integer 1 1)))
       (loop :repeat 6
             :collect (load-framebuffer-texture spec width height)))
      ((and (u:plist-p source)
            (a:set-equal (u:plist-keys source) valid-keys)
            (every #'stringp (u:plist-values source)))
       (loop :for (k v) :on source :by #'cddr
             :collect k :into result
             :collect (res.img:load v) :into result
             :finally (destructuring-bind (&key x+ x- y+ y- z+ z-) result
                        (return (list x+ x- y+ y- z+ z-)))))
      (t (error "Unsupported source for cube map texture: ~s." (name spec))))))

(defun load (name &key width height)
  (res:with-resource-cache :texture name
    (let* ((spec (find-spec name))
           (type (texture-type spec))
           (source (load-source spec type :width width :height height))
           (texture (make-texture spec type source)))
      (configure texture)
      texture)))

(live:on-recompile :texture data ()
  (a:when-let ((texture (res:find-resource :texture data)))
    (gl:delete-texture (id texture))
    (res:delete-resource :texture data)
    (load data :width (width texture) :height (height texture))))
