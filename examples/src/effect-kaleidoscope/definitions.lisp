(in-package #:pyx-examples)

(pyx:define-material effect/kaleidoscope ()
  (:shader shader:effect/kaleidoscope
   :uniforms (:time (pyx:as-uniform 'pyx:get-running-time)
              :res (pyx:as-uniform 'pyx:get-viewport-dimensions)
              :zoom 0.85
              :speed 1
              :strength 0.7
              :colorize nil
              :outline nil
              :detail 0.8)))

(pyx:define-prefab effect/kaleidoscope (:template quad)
  :render/materials '(effect/kaleidoscope))

(pyx:define-scene effect/kaleidoscope ()
  (:sub-trees (examples camera/orthographic effect/kaleidoscope)))
