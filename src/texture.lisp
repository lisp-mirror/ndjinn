(in-package #:%pyx.texture)

(defstruct (texture (:constructor %make-texture)
                    (:conc-name nil)
                    (:predicate nil)
                    (:copier nil))
  spec
  target
  id
  width
  height
  materials)

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

(defmethod update-texture ((type (eql :2d)) texture source)
  (let* ((id (gl:create-texture :texture-2d))
         (width (img:width source))
         (height (img:height source)))
    (setf (id texture) id
          (width texture) width
          (height texture) height)
    (gl:texture-storage-2d id
                           (calculate-mipmap-levels (spec texture) width height)
                           (img:internal-format source)
                           width
                           height)
    (a:when-let ((data (img:data source)))
      (gl:texture-sub-image-2d id
                               0
                               0
                               0
                               width
                               height
                               (img:pixel-format source)
                               (img:pixel-type source)
                               data))
    texture))

(defmethod update-texture ((type (eql :2d-array)) texture source)
  (let* ((id (gl:create-texture :texture-2d-array))
         (layer0 (first source))
         (width (img:width layer0))
         (height (img:height layer0)))
    (setf (id texture) id
          (width texture) width
          (height texture) height)
    (gl:texture-storage-3d id
                           (calculate-mipmap-levels (spec texture) width height)
                           (img:internal-format layer0)
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
                                       (img:width image)
                                       (img:height image)
                                       1
                                       (img:pixel-format image)
                                       (img:pixel-type image)
                                       (img:data image)))
    texture))

(defmethod update-texture ((type (eql :cube-map)) texture source)
  (let* ((id (gl:create-texture :texture-cube-map))
         (layer0 (first source))
         (width (img:width layer0))
         (height (img:height layer0)))
    (setf (id texture) id
          (width texture) width
          (height texture) height)
    (gl:texture-storage-2d id
                           (calculate-mipmap-levels (spec texture) width height)
                           (img:internal-format layer0)
                           width
                           height)
    (loop :for image :in source
          :for layer :from 0
          :do (gl:texture-sub-image-3d id
                                       0
                                       0
                                       0
                                       layer
                                       (img:width image)
                                       (img:height image)
                                       1
                                       (img:pixel-format image)
                                       (img:pixel-type image)
                                       (img:data image)))
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
  (img:load nil
            :width width
            :height height
            :pixel-format (pixel-format spec)
            :pixel-type (pixel-type spec)
            :internal-format (internal-format spec)))

(defgeneric load-source (spec type &key &allow-other-keys)
  (:method :around (spec type &key)
    (let* ((source (call-next-method))
           (source-list (a:ensure-list source)))
      (unless (and (every #'img:width source-list)
                   (every #'img:height source-list))
        (error "Texture ~s does not have a width and height set." (name spec)))
      source)))

(defmethod load-source (spec (type (eql :2d)) &key width height)
  (typecase (source spec)
    ((or null (integer 1 1))
     (load-framebuffer-texture spec width height))
    (list
     (img:load (source spec)))
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
            (every #'listp source))
       (lp:pmapcar #'img:load source))
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
            (every #'listp (u:plist-values source)))
       (loop :for (k v) :on source :by #'cddr
             :collect k :into result
             :collect v :into result
             :finally (destructuring-bind (&key x+ x- y+ y- z+ z-) result
                        (return (lp:pmapcar #'img:load
                                            (list x+ x- y+ y- z+ z-))))))
      (t (error "Unsupported source for cube map texture: ~s." (name spec))))))

(defun make-texture (spec type source)
  (let ((texture (%make-texture :spec spec :target type)))
    (update-texture type texture source)))

(defun load (name &key width height)
  (asset:with-asset-cache :texture name
    (let* ((spec (find-spec name))
           (type (texture-type spec))
           (source (load-source spec type :width width :height height))
           (texture (make-texture spec type source)))
      (configure texture)
      texture)))

(defun reload-texture-source (texture)
  (tp:submit-job
   :texture-reload
   (lambda ()
     (let ((source (load-source (spec texture)
                                (target texture)
                                :width (width texture)
                                :height (height texture))))
       (tp:enqueue :recompile `(:texture-reload (,texture ,source)))))))

(live:on-recompile :texture-reload data ()
  (destructuring-bind (texture source) data
    (asset:delete-asset :texture (name (spec texture)))
    (update-texture (target texture) texture source)
    (configure texture)))

(defmethod asset:delete-asset ((type (eql :texture)) key)
  (let ((texture (asset:find-asset type key)))
    (gl:delete-texture (id texture))))

(live:on-recompile :texture data ()
  (a:when-let ((texture (asset:find-asset :texture data)))
    (reload-texture-source texture)))
