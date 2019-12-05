(in-package #:pyx.examples)

;;; materials

(pyx:define-material base ()
  (:uniforms (:model (m4:id)
              :view (m4:id)
              :proj (m4:id))))

;;; prefabs

(pyx:define-prefab camera (:add (pyx:camera)))
