(in-package #:%pyx.texture)

(defmethod update-texture ((type (eql :2d)) texture source)
  (let* ((id (gl:gen-texture))
         (width (img:width source))
         (height (img:height source)))
    (setf (id texture) id
          (width texture) width
          (height texture) height)
    (gl:bind-texture :texture-2d id)
    (%gl:tex-storage-2d :texture-2d
                        (calculate-mipmap-levels (spec texture) width height)
                        (img:internal-format source)
                        width
                        height)
    (a:when-let ((data (img:data source)))
      (gl:tex-sub-image-2d :texture-2d
                           0
                           0
                           0
                           width
                           height
                           (img:pixel-format source)
                           (img:pixel-type source)
                           data))
    (gl:bind-texture :texture-2d 0)
    texture))

(defmethod load-source (spec (type (eql :2d)) &key width height)
  (typecase (source spec)
    ((or null (integer 1 1))
     (load-framebuffer-texture spec width height))
    (list
     (img:load (source spec)))
    (t (error "Unsupported source for 2D texture: ~s." (name spec)))))
