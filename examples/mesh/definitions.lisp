(in-package #:pyx.examples)

(pyx:define-texture mesh ()
  (:source ("mesh-albedo.png"
            "mesh-ao.png"
            "mesh-emissive.png"
            "mesh-metal-roughness.png"
            "mesh-normal.png")))

(pyx:define-material mesh ()
  (:shader shader:pbr-mesh
   :uniforms (:light.direction (v3:vec -0.7399 -0.6428 -0.1983)
              :light.color (v3:vec 1)
              :light.intensity 2
              :sampler 'mesh
              :base-color-factor (v4:vec 1)
              :metallic-factor 1
              :roughness-factor 1
              :normal-scale 1
              :normal-matrix 'pyx:resolve-normal-matrix
              :occlusion-strength 1
              :emissive-factor 1)))

(pyx:define-prefab mesh (:add (pyx:mesh pyx:render))
  :render/materials '(mesh))

(pyx:define-prefab mesh/helmet (:template mesh)
  :transform/rotate (q:orient :local :x math:pi/2)
  :mesh/file "helmet.glb"
  :mesh/name "helmet")

(pyx:define-prefab mesh-carousel (:template mesh/helmet)
  :transform/scale 17
  :transform/rotate/velocity (math:make-velocity v3:+forward+ (- math:pi/6)))

(pyx:define-scene mesh-carousel ()
  (:sub-trees (examples camera/perspective mesh-carousel)))
