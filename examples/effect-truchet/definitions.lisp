(in-package #:pyx.examples)

(pyx:define-material effect/truchet ()
  (:shader shader:effect/truchet
   :uniforms (:time 'pyx:get-running-time
              :res 'pyx:get-viewport-dimensions)))

(pyx:define-prefab effect/truchet (:template quad)
  :render/materials '(effect/truchet))

(pyx:define-scene effect/truchet ()
  (:sub-trees (examples camera/orthographic effect/truchet)))
