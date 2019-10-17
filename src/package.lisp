(in-package #:cl-user)

(defpackage #:pyx
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:log #:verbose)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:q #:origin.quat)
                    (#:m4 #:origin.mat4))
  (:use #:cl)
  ;; engine
  (:export
   #:start
   #:stop)
  ;; definitions
  (:export
   #:define-framebuffer
   #:define-material
   #:define-texture)
  ;; entity protocol
  (:export
   #:make-entity)
  ;; component types
  (:export
   #:camera
   #:identify
   #:mesh
   #:node
   #:render
   #:sprite
   #:world
   #:xform)
  ;; component api: world
  (:export
   #:world/cell-counts)
  )

(defpackage #:pyx.shader
  (:use #:shadow.glsl #:umbra.common))

(defpackage #:pyx.examples
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m4 #:origin.mat4))
  (:use #:cl)
  (:export
   #:sprite
   #:world))
