(in-package #:pyx-examples)

(pyx:define-material effect/toroidal-trip ()
  (:shader shader:effect/toroidal-trip
   :uniforms (:time (pyx:as-uniform 'pyx:get-running-time)
              :res (pyx:as-uniform 'pyx:get-viewport-dimensions))))

(pyx:define-prefab effect/toroidal-trip (:template quad)
  :render/materials '(effect/toroidal-trip))

(pyx:define-scene effect/toroidal-trip ()
  (:sub-trees (examples camera/orthographic effect/toroidal-trip)))
