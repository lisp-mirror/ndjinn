(in-package #:pyx.examples)

;;; materials

(pyx:define-material effect/toroidal-trip ()
  (:shader pyx.examples.shader:effect/toroidal-trip
   :uniforms (:time 'pyx:get-total-time
              :res 'pyx:get-viewport-dimensions)))

;;; prefabs

(pyx:define-prefab effect/toroidal-trip (:template quad)
  :render/materials '(effect/toroidal-trip))

;;; scene

(pyx:define-scene effect/toroidal-trip ()
  (:sub-trees ((examples examples)
               (camera camera/orthographic)
               (effect effect/toroidal-trip))))
