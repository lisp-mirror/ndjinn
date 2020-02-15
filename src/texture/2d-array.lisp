(in-package #:%pyx.texture)

(defmethod update-texture ((type (eql :2d-array)) texture source)
  (let* ((id (gl:gen-texture))
         (layer0 (first source))
         (width (img:width layer0))
         (height (img:height layer0)))
    (setf (id texture) id
          (width texture) width
          (height texture) height)
    (gl:bind-texture :texture-2d-array id)
    (%gl:tex-storage-3d :texture-2d-array
                        (calculate-mipmap-levels (spec texture) width height)
                        (img:internal-format layer0)
                        width
                        height
                        (length source))
    (loop :for image :in source
          :for layer :from 0
          :do (gl:tex-sub-image-3d :texture-2d-array
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
    (gl:bind-texture :texture-2d-array 0)
    texture))

(defmethod load-source (spec (type (eql :2d-array)) source &key width height)
  (cond
    ((null source)
     (list (load-framebuffer-texture spec width height)))
    ((typep source '(integer 1))
     (loop :repeat (source spec)
           :collect (load-framebuffer-texture spec width height)))
    ((and (typep source 'a:proper-list)
          (every #'listp source))
     (lp:pmapcar #'img:load source))
    (t (error "Unsupported source for 2D array texture: ~s." (name spec)))))
