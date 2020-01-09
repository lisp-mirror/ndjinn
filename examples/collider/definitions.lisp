(in-package #:pyx.examples)

;;; components

(pyx:define-component player-collision-message ()
  ()
  (:sorting :after pyx:collider/sphere))

;;; prefabs

(pyx:define-prefab collider/gate (:template mesh/sphere)
  :xform/scale 3f0
  ((collider :add (pyx:collider))
   :collider/type :sphere
   :collider/label :gate
   :collider/visualize t))

(pyx:define-prefab collider/destroyer (:template mesh/sphere)
  :xform/scale 3f0
  :xform/translate (v3:vec 30f0 0f0 0f0)
  :xform/rotate/velocity (math:make-velocity v3:+forward+ math:pi)
  ((collider :add (pyx:collider))
   :collider/type :sphere
   :collider/label :destroyer
   :collider/visualize t))

(pyx:define-prefab collider/player (:template mesh/helmet
                                    :add (player-collision-message))
  :xform/translate (v3:vec -30f0 0f0 0f0)
  :xform/translate/velocity (math:make-velocity v3:+right+ 10f0)
  :xform/rotate (q:orient :local :x math:pi/2 :y math:pi/2)
  :xform/rotate/velocity (v3:zero)
  :xform/scale 3.5f0)

(pyx:define-prefab collider/1 ()
  ((gate/top :template collider/gate)
   :xform/translate (v3:vec 0f0 5f0 0f0)
   :xform/rotate/velocity (math:make-velocity v3:+left+ math:pi))
  ((gate/bottom :template collider/gate)
   :xform/translate (v3:vec 0f0 -5f0 0f0)
   :xform/rotate/velocity (math:make-velocity v3:+right+ math:pi))
  ((destroyer :template collider/destroyer))
  ((player :template collider/player)
   ((collider :add (pyx:collider))
    :collider/type :sphere
    :collider/label :player
    :collider/visualize t
    :collider/referent (@ collider/1 player))))

;;; collision detection

(pyx:define-collider-plan collider/1 ()
  (:player (:gate :destroyer)
   :gate ()
   :destroyer ()))

(defmethod pyx:on-collision-enter ((contact1 player-collision-message)
                                   (contact2 pyx:collider))
  (ecase (pyx:collider/label contact2)
    (:gate
     (pyx:translate-entity/velocity contact1 v3:+right+ 4f0))
    (:destroyer
     (pyx:translate-entity contact1 (v3:vec -30f0 0f0 0f0) :replace-p t))))

(defmethod pyx:on-collision-exit ((contact1 player-collision-message)
                                  (contact2 pyx:collider))
  (when (eq (pyx:collider/label contact2) :gate)
    (pyx:translate-entity/velocity contact1 v3:+right+ 10f0)))

;;; scenes

(pyx:define-scene collider/1 ()
  (:collider-plan collider/1
   :prefabs (examples camera/perspective collider/1)))
