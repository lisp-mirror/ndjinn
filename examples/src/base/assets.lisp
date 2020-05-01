(in-package #:pyx-examples)

(pyx:define-asset-pool meshes ()
  :path "data/meshes"
  :filter "glb")

(pyx:define-asset-pool textures ()
  :path "data/textures"
  :filter "png")

(pyx:define-asset-pool mesh-textures ()
  :path "data/textures/mesh"
  :filter "png")

(pyx:define-asset-pool metadata ()
  :path "data/metadata")
