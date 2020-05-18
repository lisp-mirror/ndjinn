(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-material skybox ()
  (:shader shader:skybox
   :uniforms (:sampler 'res:sky)
   :features (:enable (:texture-cube-map-seamless)
              :depth-mode :lequal)))

(pyx:define-prefab skybox ()
  ((camera :template camera/perspective)
   :camera/translate-view nil)
  ((skybox :add (pyx:geometry pyx:render))
   :geometry/name 'res:skybox
   :render/materials '(skybox)
   :render/order 'background)
  ((mesh :template mesh-carousel)
   :transform/translate (v3:vec 0 0 -50)
   :render/materials '(mesh)))

(pyx:define-scene skybox ()
  (:draw-order (background default)
   :sub-trees (examples skybox)))
