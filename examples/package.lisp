(in-package #:cl-user)

(defpackage #:pyx.examples
  (:local-nicknames
   (#:a #:alexandria)
   (#:u #:golden-utils)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4)
   (#:m4 #:origin.mat4))
  (:use #:cl)
  (:export
   #:effect/kaleidoscope
   #:effect/toroidal-trip
   #:effect/truchet
   #:graph
   #:mesh/sphere
   #:mesh/helmet
   #:noise
   #:sprite
   #:texture
   #:world))

(defpackage #:pyx.examples.shader
  (:use #:shadow.glsl #:umbra.common))
