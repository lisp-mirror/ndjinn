(in-package #:pyx-examples)

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
