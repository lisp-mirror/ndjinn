(in-package #:%pyx.texture)

(defmethod update-texture ((type (eql :cube-map-array)) texture source)
  (let* ((id (gl:gen-texture))
         (first-layer (first source))
         (first-layer-face (first first-layer))
         (width (img:width first-layer-face))
         (height (img:height first-layer-face)))
    (setf (id texture) id
          (width texture) width
          (height texture) height)
    (gl:bind-texture :texture-cube-map-array id)
    (%gl:tex-storage-3d :texture-cube-map-array
                        (calculate-mipmap-levels (spec texture) width height)
                        (img:internal-format first-layer-face)
                        width
                        height
                        (* (length source) 6))
    (loop :for layer :in source
          :for layer-index :from 0
          :do (loop :for image :in layer
                    :for face-index :from 0
                    :do (gl:tex-sub-image-3d :texture-cube-map-array
                                             0
                                             0
                                             0
                                             (+ (* layer-index 6) face-index)
                                             (img:width image)
                                             (img:height image)
                                             1
                                             (img:pixel-format image)
                                             (img:pixel-type image)
                                             (img:data image))))
    (gl:bind-texture :texture-cube-map-array 0)
    texture))

(defmethod load-source (spec (type (eql :cube-map-array)) source
                        &key width height)
  (mapcar
   (lambda (x)
     (load-source spec :cube-map x :width width :height height))
   source))
