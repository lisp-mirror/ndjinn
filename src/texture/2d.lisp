(in-package #:ndjinn)

(defmethod update-texture ((type (eql :2d)) texture source)
  (let* ((id (gl:gen-texture))
         (width (width source))
         (height (height source)))
    (setf (id texture) id
          (width texture) width
          (height texture) height)
    (gl:bind-texture :texture-2d id)
    (%gl:tex-storage-2d :texture-2d
                        (calculate-texture-mipmap-levels (spec texture)
                                                         width
                                                         height)
                        (internal-format source)
                        width
                        height)
    (u:when-let ((data (data source)))
      (gl:tex-sub-image-2d :texture-2d
                           0
                           0
                           0
                           width
                           height
                           (pixel-format source)
                           (pixel-type source)
                           data))
    (gl:bind-texture :texture-2d 0)))

(defmethod load-texture-source (spec (type (eql :2d)) source &key width height)
  (typecase source
    ((or null (integer 1 1))
     (load-framebuffer-texture spec width height))
    (list
     (load-image (source spec)))
    (t (error "Unsupported source for 2D texture: ~s." (name spec)))))
