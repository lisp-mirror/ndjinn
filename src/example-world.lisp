(in-package #:pyx.examples)

(pyx:define-material world (base)
  (:shader pyx.shader:world
   :uniforms (:light.position (v3:vec 0.1 0.25 -1)
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

(pyx:define-prototype tile ()
  (pyx:mesh :file "tiles.glb"))

(pyx:define-prototype tile/floor (tile)
  (pyx:xform :scale (v3:vec 0.5 0.5 0.1))
  (pyx:mesh :name "floor")
  (pyx:render :material 'world/floor))

(pyx:define-prototype tile/wall (tile)
  (pyx:xform :translate (v3:vec 0 0 0.75)
             :scale (v3:vec 0.5 0.5 1.5))
  (pyx:mesh :name "wall")
  (pyx:render :material 'world/wall))

(pyx:define-prototype world ()
  (pyx:xform :scale 50)
  (pyx:world :width 49 :height 49))

(pyx:define-prefab world (:template world)
  :world/seed 1
  (floor (:template tile/floor)
         :mesh/instances (@ world :tiles/floor))
  (wall (:template tile/wall)
        :mesh/instances (@ world :tiles/wall)))

(pyx:define-prefab world-scene ()
  (camera (:template camera/isometric)
          :camera/mode :isometric)
  (world (:template (world))))
