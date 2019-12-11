(in-package #:pyx.examples)

;;; materials

(pyx:define-material graph ()
  (:shader pyx.examples.shader:graph
   :uniforms (:time #'pyx:total-time)))

;;; prefabs

(pyx:define-prefab graph-example (:template quad)
  :render/materials '(graph))

;;; scene

(pyx:define-scene graph ()
  (:prefabs (graph-example)))
