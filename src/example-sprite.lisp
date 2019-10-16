(in-package #:pyx.examples)

(pyx:define-texture sprites
  (:source "sprites.png"))

(pyx:define-material sprite ()
  (:shader umbra.sprite:sprite))

(defun sprite ()
  (pyx:make-entity (pyx:camera))
  (pyx:make-entity (pyx:render pyx:sprite)
    :xform/rotate/inc (v3:vec 0 0 0.1)
    :sprite/texture 'sprites
    :sprite/name "planet01"
    :render/material 'sprite))
