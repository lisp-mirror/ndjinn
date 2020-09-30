(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-material effect/truchet ()
  (:shader shader:effect/truchet
   :uniforms (:time (pyx:as-uniform 'pyx:get-running-time)
              :res (pyx:as-uniform 'pyx:get-viewport-size))))

(pyx:define-prefab effect/truchet (:template quad)
  :render/materials '(effect/truchet))

(pyx:define-scene effect/truchet ()
  (:sub-trees (examples camera/orthographic effect/truchet)))
