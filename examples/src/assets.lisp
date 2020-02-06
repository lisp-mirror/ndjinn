(in-package #:pyx.examples)

(pyx:define-asset-pool meshes (:base "data/meshes")
  (helmet "helmet.glb")
  (world-tiles "world-tiles.glb"))

(pyx:define-asset-pool textures (:base "data/textures")
  (city "city.png")
  (sprites "sprites.png"))

(pyx:define-asset-pool mesh-textures (:base "data/textures/mesh")
  (helmet-albedo "helmet-albedo.png")
  (helmet-ao "helmet-ao.png")
  (helmet-emissive "helmet-emissive.png")
  (helmet-metallic-roughness "helmet-metallic-roughness.png")
  (helmet-normal "helmet-normal.png"))

(pyx:define-asset-pool metadata (:base "data/metadata")
  (sprites "sprites.spec"))
