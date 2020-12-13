(in-package #:ndjinn)

(defmethod update-texture ((type (eql :cube-map-array)) texture source)
  (let* ((id (gl:gen-texture))
         (first-layer (first source))
         (first-layer-face (first first-layer))
         (width (image-width first-layer-face))
         (height (image-height first-layer-face)))
    (setf (texture-id texture) id
          (texture-width texture) width
          (texture-height texture) height)
    (gl:bind-texture :texture-cube-map-array id)
    (%gl:tex-storage-3d :texture-cube-map-array
                        (calculate-texture-mipmap-levels (texture-spec texture)
                                                         width
                                                         height)
                        (image-internal-format first-layer-face)
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
                                             (image-width image)
                                             (image-height image)
                                             1
                                             (image-pixel-format image)
                                             (image-pixel-type image)
                                             (image-data image))))
    (gl:bind-texture :texture-cube-map-array 0)))

(defmethod load-texture-source (spec (type (eql :cube-map-array)) source
                                &key width height)
  (lp:pmapcar
   (lambda (x)
     (load-texture-source spec :cube-map x :width width :height height))
   source))
