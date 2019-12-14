(in-package #:pyx.examples)

;;; materials

(pyx:define-material mesh ()
  (:shader pyx.shader:default
   :uniforms (:sampler 'debug)))

;;; prefabs

(pyx:define-prefab mesh (:add (pyx:mesh pyx:render))
  :xform/rotate (q:orient :local :x math:pi/2)
  :xform/rotate/velocity (math:make-velocity v3:+forward+ (- math:pi/6))
  :xform/scale 15f0
  :render/materials '(mesh))

(pyx:define-prefab mesh/sphere (:template mesh)
  :mesh/file "sphere.glb"
  :mesh/name "sphere")

(pyx:define-prefab mesh/helmet (:template mesh)
  :mesh/file "helmet.glb"
  :mesh/name "helmet")

;;; scenes

(pyx:define-scene mesh/sphere ()
  (:prefabs (examples camera/perspective mesh/sphere)))

(pyx:define-scene mesh/helmet ()
  (:prefabs (examples camera/perspective mesh/helmet)))
