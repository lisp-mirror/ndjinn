(in-package #:pyx-examples)

(pyx:define-texture mesh (:2d-array)
  (:source ((mesh-textures helmet-albedo)
            (mesh-textures helmet-ao)
            (mesh-textures helmet-emissive)
            (mesh-textures helmet-metallic-roughness)
            (mesh-textures helmet-normal))))

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
              :emissive-factor 1
              :brdf-lut 'res:brdf-lut
              :environment-sampler 'res:papermill
              :use-punctual t
              :use-ibl t)
   :features (:enable (:texture-cube-map-seamless))))

(pyx:define-prefab mesh (:add (pyx:mesh pyx:render))
  :render/materials '(mesh))

(pyx:define-prefab mesh/helmet (:template mesh)
  :transform/rotate (q:orient :local :x math:pi/2)
  :mesh/asset '(meshes helmet)
  :mesh/name "helmet")

(pyx:define-prefab mesh-carousel (:template mesh/helmet)
  :transform/scale 17
  :transform/rotate/velocity (math:make-velocity v3:+forward+ (- math:pi/6)))

(pyx:define-scene mesh-carousel ()
  (:sub-trees (examples camera/perspective mesh-carousel)))
