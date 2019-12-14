(in-package #:pyx.examples)

;;; materials

(pyx:define-material graph ()
  (:shader pyx.examples.shader:graph
   :uniforms (:time #'pyx:get-total-time)))

;;; prefabs

(pyx:define-prefab graph (:template quad)
  :render/materials '(graph))

;;; scene

(pyx:define-scene graph ()
  (:prefabs (examples graph)))
