(in-package #:pyx.examples)

;;; materials

(pyx:define-material effect/truchet ()
  (:shader pyx.examples.shader:effect/truchet
   :uniforms (:time #'pyx:get-total-time
              :res #'pyx:get-window-resolution)))

;;; prefabs

(pyx:define-prefab effect/truchet (:template quad)
  :render/materials '(effect/truchet))

;;; scene

(pyx:define-scene effect/truchet ()
  (:prefabs (camera/orthographic effect/truchet)))
