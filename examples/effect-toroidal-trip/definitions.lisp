(in-package #:pyx.examples)

;;; materials

(pyx:define-material effect/toroidal-trip ()
  (:shader pyx.examples.shader:effect/toroidal-trip
   :uniforms (:time #'pyx:get-total-time
              :res #'pyx:get-window-resolution)))

;;; prefabs

(pyx:define-prefab effect/toroidal-trip (:template quad)
  :render/materials '(effect/toroidal-trip))

;;; scene

(pyx:define-scene effect/toroidal-trip ()
  (:prefabs (camera/orthographic effect/toroidal-trip)))
