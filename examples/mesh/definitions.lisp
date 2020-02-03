(in-package #:pyx.examples)

(pyx:define-texture mesh-albedo ()
  (:source "mesh-albedo.png"))

(pyx:define-texture mesh-ao ()
  (:source "mesh-ao.png"))

(pyx:define-texture mesh-emissive ()
  (:source "mesh-emissive.png"))

(pyx:define-texture mesh-metal-roughness ()
  (:source "mesh-metal-roughness.png"))

(pyx:define-texture mesh-normal ()
  (:source "mesh-normal.png"))

(pyx:define-material mesh ()
  (:shader shader:pbr-mesh
   :uniforms (:light.direction (v3:vec -0.7399 -0.6428 -0.1983)
              :light.color (v3:vec 1)
              :light.intensity 2
              :base-color-sampler 'mesh-albedo
              :base-color-factor (v4:vec 1)
              :metallic-roughness-sampler 'mesh-metal-roughness
              :metallic-factor 1
              :roughness-factor 1
              :normal-sampler 'mesh-normal
              :normal-scale 1
              :normal-matrix 'pyx:resolve-normal-matrix
              :occlusion-sampler 'mesh-ao
              :occlusion-strength 1
              :emissive-sampler 'mesh-emissive
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
