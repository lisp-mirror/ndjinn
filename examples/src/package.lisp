(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.pyx.examples.shader
  (:local-nicknames
   (#:umbra.color #:net.mfiano.lisp.umbra.color)
   (#:umbra.graphing #:net.mfiano.lisp.umbra.graphing)
   (#:umbra.noise #:net.mfiano.lisp.umbra.noise)
   (#:umbra.sprite #:net.mfiano.lisp.umbra.sprite))
  (:use #:net.mfiano.lisp.pyx.shader)
  ;; shaders
  (:export
   #:effect/kaleidoscope
   #:effect/ocean-waves
   #:effect/toroidal-trip
   #:effect/truchet
   #:graph
   #:multi-pass
   #:noise/cellular-3d
   #:noise/cellular-fast-3d
   #:noise/cubist-3d
   #:noise/hermite-3d
   #:noise/perlin-3d
   #:noise/perlin-4d
   #:noise/perlin-improved-3d
   #:noise/perlin-surflet-3d
   #:noise/polkadot-3d
   #:noise/polkadot-box-3d
   #:noise/simplex-cellular-3d
   #:noise/simplex-perlin-3d
   #:noise/simplex-polkadot-3d
   #:noise/value-3d
   #:noise/value-4d
   #:noise/value-hermite-3d
   #:noise/value-perlin-3d
   #:pbr-mesh
   #:skybox
   #:window-rain
   #:world))

(defpackage #:net.mfiano.lisp.pyx.examples
  (:local-nicknames
   (#:dun #:net.mfiano.lisp.dungen)
   (#:math #:net.mfiano.lisp.origin)
   (#:q #:net.mfiano.lisp.origin.quat)
   (#:res #:net.mfiano.lisp.pyx.resources)
   (#:v2 #:net.mfiano.lisp.origin.vec2)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4)
   (#:m4 #:net.mfiano.lisp.origin.mat4)
   (#:pyx #:net.mfiano.lisp.pyx)
   (#:pyx.shader #:net.mfiano.lisp.pyx.shader)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:umbra.sprite #:net.mfiano.lisp.umbra.sprite)
   (#:shader #:net.mfiano.lisp.pyx.examples.shader))
  (:use #:cl)
  (:export
   #:examples))
