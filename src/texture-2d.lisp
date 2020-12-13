(in-package #:ndjinn)

(defmethod update-texture ((type (eql :2d)) texture source)
  (let* ((id (gl:gen-texture))
         (width (image-width source))
         (height (image-height source)))
    (setf (texture-id texture) id
          (texture-width texture) width
          (texture-height texture) height)
    (gl:bind-texture :texture-2d id)
    (%gl:tex-storage-2d :texture-2d
                        (calculate-texture-mipmap-levels (texture-spec texture)
                                                         width
                                                         height)
                        (image-internal-format source)
                        width
                        height)
    (u:when-let ((data (image-data source)))
      (gl:tex-sub-image-2d :texture-2d
                           0
                           0
                           0
                           width
                           height
                           (image-pixel-format source)
                           (image-pixel-type source)
                           data))
    (gl:bind-texture :texture-2d 0)))

(defmethod load-texture-source (spec (type (eql :2d)) source &key width height)
  (typecase source
    ((or null (integer 1 1))
     (load-framebuffer-texture spec width height))
    (list
     (load-image (texture-spec-source spec)))
    (t (error "Unsupported source for 2D texture: ~s."
              (texture-spec-name spec)))))
