(in-package #:pyx.examples)

;;; textures

(pyx:define-texture sprites ()
  (:source "sprites.png"))

;;; materials

(pyx:define-material sprite ()
  (:shader umbra.sprite:sprite
   :uniforms (:opacity 1.0)
   :depth-mode :lequal
   :features (:enable ()
              :disable ())))

;;; animation sequences

(pyx:define-animation-sequence sprite ()
  (pyx:sprite :duration 0.5
              :repeat-p t))

;;; prefabs

(pyx:define-prefab sprite (:add (pyx:sprite pyx:render))
  :sprite/texture 'sprites
  :render/materials '(sprite))

(pyx:define-prefab planet (:template sprite)
  :xform/scale 2f0
  :sprite/name "planet11"
  :render/materials '(sprite)
  :render/order 'background)

(pyx:define-prefab ship (:template sprite)
  :xform/scale 1.2
  :xform/translate (v3:vec 0f0 -120f0 0f0)
  :sprite/name "ship29"
  :render/order 'ships
  (exhaust (:template sprite :add (pyx:animate))
           :xform/translate (v3:vec 0f0 -145f0 0f0)
           :xform/scale (v3:vec 1f0 0.65 1f0)
           :sprite/name "exhaust01-01"
           :sprite/frames 8
           :render/order 'ships
           :animate/sequence 'sprite))

;;; pipeline

(pyx:define-pipeline sprite ()
  (:draw-order (background ships)))

;;; scene

(pyx:define-scene sprite ()
  (:pipeline sprite
   :prefabs (camera/orthographic planet ship)))
