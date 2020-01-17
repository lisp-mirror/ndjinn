(in-package #:pyx.examples)

;;; prefabs

(pyx:define-prefab camera/top-left ()
  :id/views '(top-left)
  ((camera :template camera/perspective)))

(pyx:define-prefab camera/right ()
  :id/views '(right)
  ((camera :template camera/isometric)))

;;; views

(pyx:define-view top-left ()
  (:tags (top-left)
   :x 0
   :y 1/2
   :width 1/3
   :height 1/2))

(pyx:define-view bottom-left ()
  (:tags (bottom-left)
   :x 0
   :y 0
   :width 1/3
   :height 1/2))

(pyx:define-view right ()
  (:tags (right)
   :x 1/3
   :y 0
   :width 2/3
   :height 1))

;;; scenes

(pyx:define-scene viewports ()
  (:sub-trees ((examples examples)
               (camera1 camera/top-left)
               (camera2 camera/right)
               (top-left mesh-carousel)
               (bottom-left effect/kaleidoscope)
               (right world))
   :viewports ((top-left (top-left))
               (bottom-left (bottom-left))
               (right (right)))))
