(in-package #:pyx.examples)

(pyx:define-material graph ()
  (:shader shader:graph
   :uniforms (:time (pyx:as-uniform 'pyx:get-running-time))))

(pyx:define-prefab graph (:template quad)
  :render/materials '(graph))

(pyx:define-scene graph ()
  (:sub-trees (examples graph)))
