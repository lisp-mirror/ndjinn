(in-package #:pyx-examples)

(pyx:define-prefab colliders-1 ()
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
   :id/contact 'player
   :transform/scale 4
   :transform/translate (v3:vec -30 0 0)
   :transform/translate/velocity (math:make-velocity v3:+right+ 15f0)
   :collider/target 'player
   :collider/layer 'player
   :collider/visualize nil
   ((mesh :template mesh/helmet)
    :transform/rotate (q:orient :local :x math:pi/2 :y math:pi/2))))

(pyx:define-collider-plan colliders-1 ()
  (:layers (player gate destroyer)
   :plan ((player (gate destroyer)))))

(pyx:define-collision-hook :enter (player gate)
  (pyx:translate-entity/velocity player v3:+right+ 4f0))

(pyx:define-collision-hook :enter (player destroyer)
  (pyx:translate-entity player (v3:vec -30 0 0) :replace t :instant t))

(pyx:define-collision-hook :exit (player gate)
  (pyx:translate-entity/velocity player v3:+right+ 15f0))

(pyx:define-collision-hook :picked (player)
  (format t "Player selected: ~s~%" player))

(pyx:define-scene colliders-1 ()
  (:collider-plan colliders-1
   :sub-trees (examples camera/perspective colliders-1)))

(pyx:define-prefab colliders-2 ()
  ((camera :template camera/orthographic)
   :camera/zoom 100)
  ((test-1)
   :transform/translate (v3:vec -2 0 0)
   ((cube1 :add (pyx:collider))
    :transform/translate (v3:vec -0.54 0 0)
    :transform/rotate (q:orient :local :z math:pi/4)
    :transform/rotate/velocity (math:make-velocity (v3:vec 0 0 1) math:pi/6)
    :collider/shape 'obb
    :collider/layer 'cube)
   ((cube2 :add (pyx:collider))
    :transform/translate (v3:vec 0.5 0 0)
    :collider/shape 'obb
    :collider/layer 'cube))
  ((test-2)
   :transform/translate (v3:vec 2 0 0)
   ((cube1 :add (pyx:collider))
    :transform/scale 1
    :transform/translate (v3:vec -0.55 0 0)
    :transform/rotate (q:orient :local :z math:pi/4)
    :transform/rotate/velocity (math:make-velocity (v3:vec 0 0 1) math:pi/6)
    :collider/shape 'obb
    :collider/layer 'cube)
   ((cube2 :add (pyx:collider))
    :transform/scale 0.5
    :transform/translate (v3:vec 0.5 0 0)
    :collider/shape 'sphere
    :collider/layer 'cube)))

(pyx:define-collider-plan colliders-2 ()
  (:layers (cube)
   :plan ((cube (cube)))))

(pyx:define-scene colliders-2 ()
  (:collider-plan colliders-2
   :sub-trees (examples colliders-2)))
