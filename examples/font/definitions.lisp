(in-package #:pyx.examples)

;;; helper functions

(defun update-font-text ()
  (format nil "Hello, world!~%Running time: ~,2fs" (pyx:get-total-time)))

(defun position-font-text ()
  (v2:with-components ((s (pyx:get-viewport-dimensions)))
    (v3:vec (- (/ sx 2)) 0 0)))

;;; textures

(pyx:define-texture font ()
  (:generate-mipmaps nil
   :source "core-font.png"))

;;; materials

(pyx:define-material font ()
  (:shader pyx.shader:font
   :uniforms (:time 'pyx:get-total-time
              :sampler 'font
              :color (v4:vec 1 1 1 0.3))))

;;; prefabs

(pyx:define-prefab font (:add (pyx:render pyx:font))
  :xform/scale 3f0
  :xform/translate (position-font-text)
  :font/texture 'font
  :font/geometry 'text
  :font/text 'update-font-text
  :render/materials '(font))

;;; geometry

(pyx:define-geometry-layout text ()
  (:data (:format interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(pyx:define-geometry text ()
  (:layout text
   :primitive :triangles
   :vertex-count 6))

;;; scenes

(pyx:define-scene font ()
  (:sub-trees ((examples examples)
               (camera camera/orthographic)
               (font font))))
