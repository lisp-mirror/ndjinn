(in-package #:cl-user)

(defpackage #:pyx.examples
  (:local-nicknames
   (#:a #:alexandria)
   (#:u #:golden-utils)
   (#:math #:origin)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4)
   (#:m4 #:origin.mat4)
   (#:q #:origin.quat))
  (:use #:cl)
  (:export
   #:examples))

(defpackage #:pyx.examples.shader
  (:use #:shadow.glsl #:umbra.common))
