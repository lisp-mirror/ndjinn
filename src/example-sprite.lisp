(in-package #:pyx.examples)

(pyx:define-texture sprites
  (:source "sprites.png"))

(pyx:define-material sprite ()
  (:shader umbra.sprite:sprite))

(pyx:define-prototype sprite ()
  (pyx:sprite :texture 'sprites)
  (pyx:render :material 'sprite))

(pyx:define-prefab sprite-scene ()
  (camera (:template camera/orthographic)
          :camera/clip-near 0
          :camera/clip-far 16)
  (sprite (:template sprite)
          :xform/rotate/inc (v3:vec 0 0 0.1)
          :sprite/name "planet01"))
