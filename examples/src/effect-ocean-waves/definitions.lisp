(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-material effect/ocean-waves ()
  (:shader shader:effect/ocean-waves
   :uniforms (:time (pyx:as-uniform 'pyx:get-running-time)
              :res (pyx:as-uniform 'pyx:get-viewport-size)
              :mouse (v2:vec))))

(pyx:define-prefab effect/ocean-waves (:template quad :add (mouse-input))
  :render/materials '(effect/ocean-waves))

(pyx:define-scene effect/ocean-waves ()
  (:sub-trees (examples camera/orthographic effect/ocean-waves)))
