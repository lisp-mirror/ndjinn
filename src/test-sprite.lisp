(in-package #:pyx)

(define-texture sprites
  (:source "sprites.png"))

(define-material sprite ()
  (:shader umbra.sprite:sprite))

(defun test/sprite ()
  (make-entity (camera))
  (make-entity (render sprite)
    :xform/rotate/inc (v3:vec 0 0 0.1)
    :sprite/texture 'sprites
    :sprite/name "planet01"
    :render/material 'sprite))
