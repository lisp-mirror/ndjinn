(in-package #:pyx.examples)

;;; helper functions

(defun print-frame-rate (fps)
  (u:mvlet ((div rem (truncate (truncate fps (/ 100)) 100)))
    (if (plusp fps)
        (values (format nil "~:d.~2,'0d" div rem)
                (/ 1000 fps))
        (values "?" "?"))))

(defun update-font-text ()
  (u:mvlet ((fps avg avg/10s avg/30s avg/60s (pyx::get-fps)))
    (multiple-value-call #'format nil "CPU: ~a~%~
               GPU: ~a~%~%~
               Running time: ~,2f s~%~
               Frame #: ~:d~%~
               FPS (current): ~a (~,3f ms)~%~
               FPS (10s avg.): ~a (~,3f ms)~%~
               FPS (30s avg.): ~a (~,3f ms)~%~
               FPS (1m avg.): ~a (~,3f ms)~%~
               FPS (overall): ~a (~,3f ms)"
      (pyx:get-cpu)
      (pyx:get-gpu-make/model)
      (pyx:get-total-time)
      (pyx:get-frame-count)
      (print-frame-rate fps)
      (print-frame-rate avg/10s)
      (print-frame-rate avg/30s)
      (print-frame-rate avg/60s)
      (print-frame-rate avg))))

(defun position-font-text ()
  (v2:with-components ((s (pyx:get-viewport-dimensions)))
    (v3:vec (/ sx -2) 0 0)))

;;; textures

(pyx:define-texture font ()
  (:generate-mipmaps nil
   :source "core-font.png"))

;;; materials

(pyx:define-material font ()
  (:shader pyx.shader:font
   :uniforms (:time 'pyx:get-total-time
              :sampler 'font
              :color (v4:vec 1 1 1 0.75))))

;;; prefabs

(pyx:define-prefab font (:add (pyx:render pyx:font))
  :xform/scale 0.5
  :font/texture 'font
  :font/geometry 'text
  :font/text 'update-font-text
  :font/position :top-left
  :font/offset (v2:vec 5 5)
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

;;; render passes

(pyx:define-render-pass font ()
  (:clear-color (v4:vec 0 0 0 1)
   :clear-buffers (:color :depth)))

;;; scenes

(pyx:define-scene font ()
  (:sub-trees (examples camera/orthographic font)))
