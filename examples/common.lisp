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
  :xform/translate (v3:vec 0f0 0f0 50f0))

(pyx:define-prefab camera/orthographic (:add (pyx:camera))
  :xform/translate (v3:vec 0f0 0f0 1f0)
  :camera/mode :orthographic
  :camera/clip-near 0f0
  :camera/clip-far 16f0)

(pyx:define-prefab camera/isometric (:add (pyx:camera))
  :xform/translate (v3:vec 0f0 0f0 1f0)
  :camera/mode :isometric
  :camera/clip-near -1000f0
  :camera/clip-far 1000f0)

(pyx:define-prefab quad (:add (pyx:mesh pyx:render))
  :mesh/file "plane.glb"
  :mesh/name "plane"
  :render/materials '(quad))
