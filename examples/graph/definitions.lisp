(in-package #:pyx.examples)

;;; materials

(pyx:define-material graph ()
  (:shader pyx.examples.shader:graph
   :uniforms (:frame-count 'pyx:get-frame-count
              :frame-time 'pyx:get-frame-time)))

;;; prefabs

(pyx:define-prefab graph (:template quad)
  :render/materials '(graph))

;;; scene

(pyx:define-scene graph ()
  (:sub-trees ((examples examples)
               (graph graph))))
