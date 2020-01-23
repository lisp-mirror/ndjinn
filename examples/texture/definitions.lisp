(in-package #:pyx.examples)

;;; prefabs

(pyx:define-prefab texture-example (:template quad))

;;; scene

(pyx:define-scene texture ()
  (:sub-trees (examples texture-example)))
