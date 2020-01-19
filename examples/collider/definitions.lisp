(in-package #:pyx.examples)

;;; components

(pyx:define-component player-collision-message ()
  ()
  (:sorting :after pyx:collider))

;;; prefabs

(pyx:define-prefab collider ()
  ((gate/top :add (pyx:collider/sphere))
   :xform/translate (v3:vec 0f0 8f0 0f0)
   :xform/scale 5f0
   :collider/layer :gate
   :collider/visualize t)
  ((gate/bottom :add (pyx:collider/sphere))
   :xform/translate (v3:vec 0f0 -8f0 0f0)
   :xform/scale 5f0
   :collider/layer :gate
   :collider/visualize t)
  ((destroyer :add (pyx:collider/sphere))
   :xform/scale 3f0
   :xform/translate (v3:vec 30f0 0f0 0f0)
   :collider/layer :destroyer
   :collider/visualize t)
  ((player :add (pyx:collider/sphere))
   :id/contact 'player
   :xform/scale 4f0
   :xform/translate (v3:vec -30f0 0f0 0f0)
   :xform/translate/velocity (math:make-velocity v3:+right+ 15f0)
   :collider/target 'player
   :collider/layer :player
   :collider/visualize t
   ((mesh :template mesh/helmet)
    :xform/rotate (q:orient :local :x math:pi/2 :y math:pi/2))))

;;; collision detection

(pyx:define-collider-plan collider ()
  (:player (:gate :destroyer)
   :gate nil
   :destroyer nil))

(pyx:define-collision-hook :enter (player :gate)
  (pyx:translate-entity/velocity player v3:+right+ 3f0))

(pyx:define-collision-hook :enter (player :destroyer)
  (pyx:translate-entity player (v3:vec -30f0 0f0 0f0) :replace-p t))

(pyx:define-collision-hook :exit (player :gate)
  (pyx:translate-entity/velocity player v3:+right+ 15f0))

;;; scenes

(pyx:define-scene collider ()
  (:collider-plan collider
   :sub-trees ((examples examples)
               (camera camera/perspective)
               (collider collider))))
