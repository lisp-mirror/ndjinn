(in-package #:pyx.examples)

;;; materials

(pyx:define-material effect/kaleidoscope ()
  (:shader pyx.examples.shader:effect/kaleidoscope
   :uniforms (:frame-count 'pyx:get-frame-count
              :frame-time 'pyx:get-frame-time
              :res 'pyx:get-viewport-dimensions
              :zoom 0.85
              :speed 1
              :strength 0.7
              :colorize nil
              :outline nil
              :detail 0.8)))

;;; prefabs

(pyx:define-prefab effect/kaleidoscope (:template quad)
  :render/materials '(effect/kaleidoscope))

;;; scene

(pyx:define-scene effect/kaleidoscope ()
  (:sub-trees ((examples examples)
               (camera camera/orthographic)
               (effect effect/kaleidoscope))))
