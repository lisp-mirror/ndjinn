(in-package #:cl-user)

(defpackage #:pyx
  (:local-nicknames
   (#:a #:alexandria)
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
   #:stop
   #:deploy)
  ;; definitions
  (:export
   #:define-animation-sequence
   #:define-animation-state
   #:define-animation-state-hook
   #:define-component
   #:define-framebuffer
   #:define-geometry
   #:define-geometry-layout
   #:define-groups
   #:define-material
   #:define-prefab
   #:define-prototype
   #:define-query
   #:define-query-types
   #:define-texture)
  ;; input
  (:export
   #:get-gamepad-analog
   #:get-mouse-position
   #:get-mouse-scroll
   #:input-enabled-p
   #:input-enter-p
   #:input-exit-p
   #:on-gamepad-analog-move
   #:on-gamepad-attach
   #:on-gamepad-detach)
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
   #:animate
   #:camera
   #:group
   #:identify
   #:mesh
   #:node
   #:render
   #:sprite
   #:world
   #:xform)
  ;; animation states
  (:export
   #:sprite
   #:fade
   #:rotate)
  ;; groups
  (:export
   #:group-join
   #:group-leave))

(defpackage #:pyx.shader
  (:use #:shadow.glsl #:umbra.common))

(defpackage #:pyx.examples
  (:local-nicknames
   (#:a #:alexandria)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4)
   (#:m4 #:origin.mat4))
  (:use #:cl)
  (:export
   #:sprite-scene
   #:world-scene))
