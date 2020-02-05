(in-package #:pyx.examples)

(pyx:define-texture mesh (:2d-array)
  (:source ("data/textures/mesh/mesh-albedo.png"
            "data/textures/mesh/mesh-ao.png"
            "data/textures/mesh/mesh-emissive.png"
            "data/textures/mesh/mesh-metal-roughness.png"
            "data/textures/mesh/mesh-normal.png")))

(pyx:define-texture brdf-lut ()
  (:source "data/textures/brdf-lut.png"))

(pyx:define-texture env-diffuse (:cube-map)
  (:source (:x+ "data/environments/papermill-ldr-diffuse-right.png"
            :x- "data/environments/papermill-ldr-diffuse-left.png"
            :y+ "data/environments/papermill-ldr-diffuse-bottom.png"
            :y- "data/environments/papermill-ldr-diffuse-top.png"
            :z+ "data/environments/papermill-ldr-diffuse-front.png"
            :z- "data/environments/papermill-ldr-diffuse-back.png")))

(pyx:define-texture env-specular (:cube-map)
  (:min-filter :linear-mipmap-linear
   :source (:x+ "data/environments/papermill-ldr-specular-right.png"
            :x- "data/environments/papermill-ldr-specular-left.png"
            :y+ "data/environments/papermill-ldr-specular-bottom.png"
            :y- "data/environments/papermill-ldr-specular-top.png"
            :z+ "data/environments/papermill-ldr-specular-front.png"
            :z- "data/environments/papermill-ldr-specular-back.png")))

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
              :brdf-lut 'brdf-lut
              :diffuse-sampler 'env-diffuse
              :specular-sampler 'env-specular
              :use-punctual t
              :use-ibl t)
   :features (:enable (:texture-cube-map-seamless))))

(pyx:define-prefab mesh (:add (pyx:mesh pyx:render))
  :render/materials '(mesh))

(pyx:define-prefab mesh/helmet (:template mesh)
  :transform/rotate (q:orient :local :x math:pi/2)
  :mesh/file "data/mesh/helmet.glb"
  :mesh/name "helmet")

(pyx:define-prefab mesh-carousel (:template mesh/helmet)
  :transform/scale 17
  :transform/rotate/velocity (math:make-velocity v3:+forward+ (- math:pi/6)))

(pyx:define-scene mesh-carousel ()
  (:sub-trees (examples camera/perspective mesh-carousel)))
