(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-prefab colliders2/object (:add (pyx:collider))
  :collider/shape 'obb
  :collider/layer 'object)

(pyx:define-prefab colliders2/left (:template colliders2/object)
  :transform/translate (v3:vec -0.55 0 0)
  :transform/rotate/velocity (math:make-velocity v3:+forward+ math:pi/6))

(pyx:define-prefab colliders2/right (:template colliders2/object)
  :transform/translate (v3:vec 0.5 0 0))

(pyx:define-prefab colliders2 ()
  ((camera :template camera/orthographic)
   :camera/zoom 100)
  ((test1)
   :transform/translate (v3:vec -2 0 0)
   ((left :template colliders2/left))
   ((right :template colliders2/right)))
  ((test2)
   :transform/translate (v3:vec 2 0 0)
   ((left :template colliders2/left))
   ((right :template colliders2/right)
    :transform/scale 0.5
    :collider/shape 'sphere)))

(pyx:define-collider-plan colliders2 ()
  (:layers (object)
   :plan ((object (object)))))

(pyx:define-scene colliders2 ()
  (:collider-plan colliders2
   :sub-trees (examples colliders2)))
