(in-package #:pyx-examples)

(pyx:define-context examples ()
  (:scene examples))

(pyx:define-material full-quad ()
  (:shader pyx.shader:full-quad
   :uniforms (:sampler 'res:debug)))

(pyx:define-material quad ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'res:debug)))

(pyx:define-prefab camera (:add (pyx:camera))
  :camera/debug t)

(pyx:define-prefab camera/perspective (:template camera)
  :camera/debug-speed 0.1
  :transform/translate (v3:vec 0 0 50))

(pyx:define-prefab camera/orthographic (:template camera)
  :camera/debug-speed 4
  :transform/translate (v3:vec 0 0 1)
  :camera/mode :orthographic
  :camera/clip-near 0
  :camera/clip-far 16)

(pyx:define-prefab camera/isometric (:template camera)
  :transform/translate (v3:vec 0 0 1)
  :camera/debug-speed 4
  :camera/mode :isometric
  :camera/clip-near -1000
  :camera/clip-far 1000)

(pyx:define-prefab camera/free-look (:template camera/perspective)
  :camera/free-look t)

(pyx:define-prefab quad (:add (pyx:geometry pyx:render))
  :geometry/name 'res:quad
  :render/materials '(quad))

(pyx:define-prefab full-quad (:template quad)
  :render/materials '(full-quad))

(pyx:define-prefab examples (:add (scene-switcher)))

(pyx:define-scene examples ()
  (:sub-trees (examples)))
