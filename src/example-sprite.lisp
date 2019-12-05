(in-package #:pyx.examples)

;;; textures

(pyx:define-texture sprites
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

;;; groups

(pyx:define-groups ()
  (background :draw-order 0)
  (ships :draw-order 1))

;;; prefabs

(pyx:define-prefab sprite (:add (pyx:sprite pyx:render))
  :sprite/texture 'sprites
  :render/material 'sprite)

(pyx:define-prefab planet (:template sprite)
  :group/name 'background
  :xform/scale 2
  :sprite/name "planet11")

(pyx:define-prefab ship ()
  :group/name 'ships
  :xform/scale 1.2
  :xform/translate (v3:vec 0 -120 0)
  (body (:template sprite)
        :sprite/name "ship29")
  (exhaust (:template sprite :add (pyx:animate))
           :xform/translate (v3:vec 0 -145 0)
           :xform/scale (v3:vec 1 0.65 1)
           :sprite/name "exhaust01-01"
           :sprite/frames 8
           :animate/sequence 'sprite))

(pyx:define-prefab sprite-scene ()
  (camera (:template camera)
          :camera/mode :orthographic
          :camera/clip-near 0
          :camera/clip-far 16)
  (planet (:template planet))
  (ship (:template ship)))
