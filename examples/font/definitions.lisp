(in-package #:pyx.examples)

;;; helper functions

(defun print-time (time &optional (padding 12))
  (u:mvlet ((div rem (truncate (truncate time (/ 100)) 100)))
    (format nil "~v:d.~2,'0d" padding div rem)))

(defun print-frame-rate (fps)
  (if (plusp fps)
      (values (print-time fps)
              (print-time (/ 1000 fps) 3))
      (values "              -" "     -")))

(defun update-font-text ()
  (u:mvlet ((fps avg avg/10s avg/30s avg/60s (pyx::get-fps)))
    (multiple-value-call #'format nil "CPU: ~a~%~
               GPU: ~a~%~%~
               Running time: ~,2f s~%~
               Frame #: ~:d~%~
               FPS (current): ~a (~a ms)~%~
               FPS (10s avg.): ~a (~a ms)~%~
               FPS (30s avg.): ~a (~a ms)~%~
               FPS (1m avg.): ~a (~a ms)~%~
               FPS (overall): ~a (~a ms)"
      (pyx:get-cpu)
      (pyx:get-gpu-make/model)
      (pyx:get-total-time)
      (pyx:get-frame-count)
      (print-frame-rate fps)
      (print-frame-rate avg/10s)
      (print-frame-rate avg/30s)
      (print-frame-rate avg/60s)
      (print-frame-rate avg))))

(defun print-hardware-info ()
  (format nil "CPU: ~a~%GPU: ~a"
          (pyx:get-cpu)
          (pyx:get-gpu-make/model)))

(defun print-fps-labels ()
  (format nil
          "Counters~%Elapsed Time:~%Frames Drawn:~%~%~
           Frame Rates~%Current:~%Last 10s:~%Last 30s:~%Last 1m:~%Overall:"))

(defun print-fps-times ()
  (u:mvlet ((fps avg avg/10s avg/30s avg/1m (pyx:get-fps)))
    (multiple-value-call #'format nil
      "~a s ~%~
       ~15:d~%~%~%~
       ~a / ~,3f ms~%~
       ~a / ~,3f ms~%~
       ~a / ~,3f ms~%~
       ~a / ~,3f ms~%~
       ~a / ~,3f ms~%"
      (print-time (pyx:get-total-time))
      (pyx:get-frame-count)
      (print-frame-rate fps)
      (print-frame-rate avg/10s)
      (print-frame-rate avg/30s)
      (print-frame-rate avg/1m)
      (print-frame-rate avg))))

;;; textures

(pyx:define-texture font ()
  (:generate-mipmaps nil
   :source "core-font.png"))

;;; materials

(pyx:define-material font ()
  (:shader pyx.shader:font
   :uniforms (:res 'pyx:get-viewport-dimensions
              :sampler 'font
              :color (v4:vec 0 1 0 0.75))))

;;; prefabs

(pyx:define-prefab debug-info (:add (pyx:render pyx:font))
  :xform/scale 1
  :font/texture 'font
  :font/geometry 'text
  :font/text 'update-font-text
  :font/position :top-left
  :font/offset (v2:vec 10 -10)
  :render/materials '(font))

(pyx:define-prefab debug-info ()
  ((hardware-info :add (pyx:render pyx:font))
   :font/geometry 'text
   :font/texture 'font
   :font/text 'print-hardware-info
   :font/position :top-left
   :font/offset (v2:vec 0.5 -0.5)
   :render/materials '(font))
  ((fps-labels :add (pyx:render pyx:font))
   :font/geometry 'text
   :font/texture 'font
   :font/text 'print-fps-labels
   :font/position :top-left
   :font/offset (v2:vec 0.5 -3.5)
   :render/materials '(font))
  ((fps-times :add (pyx:render pyx:font))
   :font/geometry 'text
   :font/texture 'font
   :font/text 'print-fps-times
   :font/position :top-left
   :font/offset (v2:vec 10 -4.5)
   :render/materials '(font)))

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
  (:sub-trees (examples camera/orthographic debug-info)))
