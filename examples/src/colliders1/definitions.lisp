(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-prefab colliders1 ()
  ((gate/top :add (pyx:collider))
   :transform/translate (v3:vec 0 8 0)
   :transform/scale 5
   :collider/layer 'gate)
  ((gate/bottom :add (pyx:collider))
   :transform/translate (v3:vec 0 -8 0)
   :transform/scale 5
   :collider/layer 'gate)
  ((destroyer :add (pyx:collider))
   :transform/scale 3
   :transform/translate (v3:vec 30 0 0)
   :transform/rotate/velocity (math:make-velocity v3:+down+ 5f0)
   :collider/layer 'destroyer)
  ((player :add (pyx:collider))
   :transform/scale 4
   :transform/translate (v3:vec -30 0 0)
   :transform/translate/velocity (math:make-velocity v3:+right+ 15f0)
   :collider/layer 'player
   :collider/visualize nil
   ((mesh :template mesh/helmet)
    :transform/rotate (q:orient :local :x math:pi/2 :y math:pi/2))))

(pyx:define-collider-plan colliders1 ()
  (:layers (player gate destroyer)
   :plan ((player (gate destroyer)))))

(pyx:define-collision-hook :enter (player gate)
  (pyx:translate-entity/velocity player v3:+right+ 4f0))

(pyx:define-collision-hook :enter (player destroyer)
  (pyx:translate-entity player (v3:vec -30 0 0) :replace t :instant t))

(pyx:define-collision-hook :exit (player gate)
  (pyx:translate-entity/velocity player v3:+right+ 15f0))

(pyx:define-collision-hook :picked (player)
  (pyx:send-to-repl (list player) :comment "Picked"))

(pyx:define-scene colliders1 ()
  (:collider-plan colliders1
   :sub-trees (examples camera/perspective colliders1)))
