(in-package :pyx.examples)

;;; textures

(pyx:define-texture debug ()
  (:source "debug.png"))

;;; materials

(pyx:define-material quad ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'debug)))

;;; prefabs

(pyx:define-prefab texture-example ()
  (quad (:add (pyx:mesh pyx:render))
        :mesh/file "plane.glb"
        :mesh/name "plane"
        :render/materials '(quad)))

;;; scene

(pyx:define-scene texture ()
  (:prefabs (texture-example)))
