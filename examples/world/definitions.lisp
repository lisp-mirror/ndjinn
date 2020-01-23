(in-package #:pyx.examples)

;;; materials

(pyx:define-material world ()
  (:shader pyx.examples.shader:world
   :uniforms (:light.position (v3:vec 0.1 0.25 -1f0)
              :light.ambient (v4:vec 0.01 0.01 0.01 0.01)
              :light.diffuse (v4:vec 0.5 0.5 0.5 0.5)
              :light.specular (v4:vec 0.2 0.2 0.2 0.2)
              :material.ambient (v4:one)
              :material.diffuse (v4:one)
              :material.specular (v4:one)
              :material.shininess 10
              :opacity 1.0)))

(pyx:define-material world/floor (world)
  (:uniforms (:cell-type 0)))

(pyx:define-material world/wall (world)
  (:uniforms (:cell-type 1)))

;;; prefabs

(pyx:define-prefab tile (:add (pyx:mesh pyx:render))
  :mesh/file "tiles.glb")

(pyx:define-prefab tile/floor (:template tile)
  :xform/scale (v3:vec 0.5 0.5 0.1)
  :mesh/name "floor"
  :render/materials '(world/floor))

(pyx:define-prefab tile/wall (:template tile)
  :xform/translate (v3:vec 0f0 0f0 1.25)
  :xform/scale (v3:vec 0.5 0.5 1.25)
  :mesh/name "wall"
  :render/materials '(world/wall))

(pyx:define-prefab world (:add (world))
  :xform/scale 40f0
  :world/width 49
  :world/height 49
  :world/seed 1
  ((floor :template tile/floor)
   :mesh/instances (@ world :tiles/floor))
  ((wall :template tile/wall)
   :mesh/instances (@ world :tiles/wall)))

;;; scene

(pyx:define-scene world ()
  (:sub-trees (examples camera/isometric world)))
