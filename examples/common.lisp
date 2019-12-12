(in-package #:pyx.examples)

;;; textures

(pyx:define-texture debug ()
  (:source "debug.png"))

;;; materials

(pyx:define-material quad ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'debug)))

;;; prefabs

(pyx:define-prefab camera/perspective (:add (pyx:camera))
  :xform/translate (v3:vec 0 0 50))

(pyx:define-prefab camera/orthographic (:add (pyx:camera))
  :xform/translate (v3:vec 0 0 1)
  :camera/mode :orthographic
  :camera/clip-near 0
  :camera/clip-far 16)

(pyx:define-prefab camera/isometric (:add (pyx:camera))
  :xform/translate (v3:vec 0 0 1)
  :camera/mode :isometric
  :camera/clip-near -1000
  :camera/clip-far 1000)

(pyx:define-prefab quad (:add (pyx:mesh pyx:render))
  :mesh/file "plane.glb"
  :mesh/name "plane"
  :render/materials '(quad))
