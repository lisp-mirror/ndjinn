(in-package #:pyx.examples)

;;; prefabs

(pyx:define-prefab camera/top-left ()
  :id/tags '(top-left)
  ((camera :template camera/perspective)))

(pyx:define-prefab camera/bottom-left ()
  :id/tags '(bottom-left)
  ((camera :template camera/orthographic)))

(pyx:define-prefab camera/right ()
  :id/tags '(right)
  ((camera :template camera/isometric)))

;;; views

(pyx:define-view top-left ()
  (:tags (top-left)
   :x 0f0
   :y 0.5f0
   :width 0.25f0
   :height 0.5f0))

(pyx:define-view bottom-left ()
  (:tags (bottom-left)
   :x 0f0
   :y 0f0
   :width 0.25f0
   :height 0.5f0))

(pyx:define-view right ()
  (:tags (right)
   :x 0.25f0
   :y 0f0
   :width 0.75f0
   :height 1f0))

;;; scenes

(pyx:define-scene viewports ()
  (:sub-trees ((examples examples)
               (camera1 camera/top-left)
               (camera2 camera/bottom-left)
               (camera3 camera/right)
               (a mesh-carousel)
               (b effect/ocean-waves)
               (c world))
   :viewports ((top-left (a))
               (bottom-left (b))
               (right (c)))))
