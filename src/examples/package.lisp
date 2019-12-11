(in-package #:cl-user)

(defpackage #:pyx.examples
  (:local-nicknames
   (#:a #:alexandria)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4)
   (#:m4 #:origin.mat4))
  (:use #:cl)
  (:export
   #:graph
   #:mesh/1
   #:mesh/2
   #:noise
   #:sprite
   #:texture
   #:world))

(defpackage #:pyx.examples.shader
  (:use #:shadow.glsl #:umbra.common))
