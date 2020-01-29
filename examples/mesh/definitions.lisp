(in-package #:pyx.examples)

(pyx:define-prefab mesh (:add (pyx:mesh pyx:render))
  :render/materials '(ext:mesh))

(pyx:define-prefab mesh/helmet (:template mesh)
  :transform/rotate (q:orient :local :x math:pi/2)
  :mesh/file "helmet.glb"
  :mesh/name "helmet")

(pyx:define-prefab mesh-carousel (:template mesh/helmet)
  :transform/scale 17
  :transform/rotate/velocity (math:make-velocity v3:+forward+ (- math:pi/6)))

(pyx:define-scene mesh-carousel ()
  (:sub-trees (examples camera/perspective mesh-carousel)))
