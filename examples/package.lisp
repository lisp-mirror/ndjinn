(in-package #:cl-user)

(defpackage #:pyx.examples.shader
  (:use #:pyx.shader))

(defpackage #:pyx.examples
  (:local-nicknames
   (#:a #:alexandria)
   (#:anim #:%pyx.animation)
   (#:ext #:pyx.extension)
   (#:math #:origin)
   (#:q #:origin.quat)
   (#:res #:%pyx.resource)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4)
   (#:m4 #:origin.mat4)
   (#:u #:golden-utils)
   (#:shader #:pyx.examples.shader))
  (:use #:cl)
  (:export
   #:examples))
