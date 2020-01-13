(in-package #:pyx.examples)

;;; prefabs

(pyx:define-prefab mesh (:add (pyx:mesh pyx:render))
  :render/materials '(pyx:mesh))

(pyx:define-prefab mesh/sphere (:template mesh)
  :mesh/file "sphere.glb"
  :mesh/name "sphere")

(pyx:define-prefab mesh/helmet (:template mesh)
  :xform/rotate (q:orient :local :x math:pi/2)
  :mesh/file "helmet.glb"
  :mesh/name "helmet")

(pyx:define-prefab mesh-carousel (:template mesh/helmet)
  :xform/scale 15f0
  :xform/rotate/velocity (math:make-velocity v3:+forward+ (- math:pi/6)))

;;; scenes

(pyx:define-scene mesh-carousel ()
  (:sub-trees ((examples examples)
               (camera camera/perspective)
               (mesh mesh-carousel))))
