(in-package #:cl-user)

(defpackage #:pyx
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:ff #:filtered-functions)
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
   #:define-component
   #:define-framebuffer
   #:define-geometry
   #:define-geometry-layout
   #:define-material
   #:define-prefab
   #:define-prototype
   #:define-query
   #:define-query-types
   #:define-texture)
  ;; entity protocol
  (:export
   #:delete-entity
   #:make-entity
   #:modify-entity
   #:on-entity-deleted)
  ;; component protocol
  (:export
   #:add-component
   #:has-component-p
   #:on-component-added
   #:on-component-removed
   #:remove-component
   #:remove-components)
  ;; component types
  (:export
   #:camera
   #:identify
   #:mesh
   #:node
   #:render
   #:sprite
   #:world
   #:xform))

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
   #:sprite-scene
   #:world-scene))
