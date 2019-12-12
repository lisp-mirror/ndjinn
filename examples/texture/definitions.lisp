(in-package #:pyx.examples)

;;; prefabs

(pyx:define-prefab texture-example (:template quad))

;;; scene

(pyx:define-scene texture ()
  (:prefabs (texture-example)))
