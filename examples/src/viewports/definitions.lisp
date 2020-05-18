(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-prefab camera/top-left ()
  :id/views '(top-left)
  ((camera :template camera/perspective)))

(pyx:define-prefab camera/right ()
  :id/views '(right)
  ((camera :template camera/isometric)))

(pyx:define-viewport top-left ()
  (:x 0 :y 1/2 :width 1/3 :height 1/2))

(pyx:define-viewport bottom-left ()
  (:x 0 :y 0 :width 1/3 :height 1/2))

(pyx:define-viewport right ()
  (:x 1/3 :y 0 :width 2/3 :height 1))

(pyx:define-scene viewports ()
  (:sub-trees (examples
               camera/top-left
               camera/right
               mesh-carousel
               effect/kaleidoscope
               world)
   :viewports ((top-left (mesh-carousel))
               (bottom-left (effect/kaleidoscope))
               (right (world)))))
