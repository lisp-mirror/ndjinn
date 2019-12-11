(in-package #:pyx.examples)

;;; prefabs

(pyx:define-prefab texture-example (:template full-screen-quad))

;;; scene

(pyx:define-scene texture ()
  (:prefabs (texture-example)))
