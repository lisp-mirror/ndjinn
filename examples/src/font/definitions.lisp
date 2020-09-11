(in-package #:net.mfiano.lisp.pyx.examples)

(defun seconds->duration (seconds)
  (u:mvlet* ((days d-rem (floor seconds 86400))
             (hours h-rem (floor d-rem 3600))
             (minutes m-rem (floor h-rem 60))
             (seconds (floor m-rem)))
    (format nil "~dd ~dh ~dm ~ds" days hours minutes seconds)))

(defun print-time (time)
  (u:mvlet ((div rem (truncate (truncate time (/ 100)) 100)))
    (format nil "~:d.~2,'0d" div rem)))

(defun print-frame-rate (fps)
  (if (plusp fps)
      (values (print-time fps)
              (/ 1000 fps))
      (values "-" "-")))

(defun print-fps-labels ()
  (format nil
          "CPU: ~a~%GPU: ~a~%~%~
           Counters~%Elapsed Time:~%Frames Drawn:~%~%~
           Frame Rates~%Current:~%Last 10s:~%Last 30s:~%Last 1m:~%Overall:"
          (pyx:get-hardware-info :cpu)
          (pyx:get-hardware-info :gpu-make/model)))

(defun print-fps-times ()
  (u:mvlet ((fps avg avg/10s avg/30s avg/1m (pyx:get-fps)))
    (multiple-value-call #'format nil
      "~a~%~
       ~:d~%~%~%~
       ~a / ~,3f ms~%~
       ~a / ~,3f ms~%~
       ~a / ~,3f ms~%~
       ~a / ~,3f ms~%~
       ~a / ~,3f ms~%"
      (seconds->duration (pyx:get-running-time))
      (pyx:get-frame-count)
      (print-frame-rate fps)
      (print-frame-rate avg/10s)
      (print-frame-rate avg/30s)
      (print-frame-rate avg/1m)
      (print-frame-rate avg))))

(pyx:define-texture font ()
  (:generate-mipmaps nil
   :source "data/font/core-font.png"))

(pyx:define-material font ()
  (:shader pyx.shader:font
   :uniforms (:sampler 'res:font
              :color (v4:vec 0 1 0 0.75))))

(pyx:define-prefab debug-info ()
  ((fps-labels :add (pyx:render pyx:font))
   :font/asset '(res:metadata res:font)
   :font/text 'print-fps-labels
   :font/position :top-left
   :font/offset (v2:vec 0.5 0.5)
   :render/materials '(font))
  ((fps-times :add (pyx:render pyx:font))
   :font/asset '(res:metadata res:font)
   :font/text 'print-fps-times
   :font/position :top-left
   :font/offset (v2:vec 12 4.5)
   :render/materials '(font)))

(pyx:define-scene font ()
  (:sub-trees (examples camera/orthographic debug-info)))
