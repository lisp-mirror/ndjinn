(in-package #:pyx.examples)

;;; materials

(pyx:define-material mesh ()
  (:shader pyx.shader:default
   :uniforms (:sampler 'debug)))

;;; prefabs

(pyx:define-prefab mesh (:add (pyx:mesh pyx:render))
  :xform/rotate (v3:vec (/ pi 2) 0 0)
  :xform/rotate/inc (v3:vec 0 0 -0.01)
  :xform/scale 15
  :render/materials '(mesh))

(pyx:define-prefab mesh/1 (:template mesh)
  :mesh/file "sphere.glb"
  :mesh/name "sphere")

(pyx:define-prefab mesh/2 (:template mesh)
  :mesh/file "helmet.glb"
  :mesh/name "helmet")

;;; scenes

(pyx:define-scene mesh/1 ()
  (:prefabs (camera/perspective mesh/1)))

(pyx:define-scene mesh/2 ()
  (:prefabs (camera/perspective mesh/2)))
