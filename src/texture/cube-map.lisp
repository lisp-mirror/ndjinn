(in-package #:ndjinn)

(defmethod update-texture ((type (eql :cube-map)) texture source)
  (let* ((id (gl:gen-texture))
         (layer0 (first source))
         (width (width layer0))
         (height (height layer0))
         (faces '(:texture-cube-map-positive-x
                  :texture-cube-map-negative-x
                  :texture-cube-map-positive-y
                  :texture-cube-map-negative-y
                  :texture-cube-map-positive-z
                  :texture-cube-map-negative-z)))
    (setf (id texture) id
          (width texture) width
          (height texture) height)
    (gl:bind-texture :texture-cube-map id)
    (%gl:tex-storage-2d :texture-cube-map
                        (calculate-texture-mipmap-levels (spec texture)
                                                         width
                                                         height)
                        (internal-format layer0)
                        width
                        height)
    (loop :for image :in source
          :for face :in faces
          :do (path image)
              (gl:tex-sub-image-2d face
                                   0
                                   0
                                   0
                                   (width image)
                                   (height image)
                                   (pixel-format image)
                                   (pixel-type image)
                                   (data image)))
    (gl:bind-texture :texture-cube-map 0)))

(defmethod load-texture-source (spec (type (eql :cube-map)) source
                                &key width height)
  (let ((valid-keys '(:x+ :x- :y+ :y- :z+ :z-)))
    (cond
      ((typep source '(or null (integer 1 1)))
       (loop :repeat 6
             :collect (load-framebuffer-texture spec width height)))
      ((and (u:plist-p source)
            (u:set-equal (u:plist-keys source) valid-keys)
            (every #'listp (u:plist-values source)))
       (loop :for (k v) :on source :by #'cddr
             :collect k :into result
             :collect v :into result
             :finally (destructuring-bind (&key x+ x- y+ y- z+ z-) result
                        (return (lp:pmapcar #'load-image
                                            (list x+ x- y+ y- z+ z-))))))
      (t (error "Unsupported source for cube map texture: ~s." (name spec))))))
