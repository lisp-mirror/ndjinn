(in-package #:cl-user)

(defpackage #:pyx
  (:local-nicknames
   (#:a #:alexandria)
   (#:u #:golden-utils)
   (#:ff #:filtered-functions)
   (#:log #:verbose)
   (#:math #:origin)
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
  ;; common
  (:export
   #:meta
   #:get-window-resolution
   #:get-total-time)
  ;; definitions
  ;; scenes
  (:export
   #:get-current-scene-name
   #:load-scene
   #:switch-scene)
  (:export
   #:define-animation-sequence
   #:define-animation-state
   #:define-animation-state-hook
   #:define-collider-plan
   #:define-component
   #:define-framebuffer
   #:define-geometry
   #:define-geometry-layout
   #:define-hook
   #:define-material
   #:define-prefab
   #:define-query
   #:define-query-types
   #:define-render-pass
   #:define-scene
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
   #:make-entity)
  ;; component protocol
  (:export
   #:attach-component
   #:detach-component
   #:detach-components
   #:has-component-p)
  ;; component types
  (:export
   #:animate
   #:camera
   #:collider
   #:group
   #:id
   #:mesh
   #:node
   #:render
   #:sprite
   #:xform)
  ;; resources
  (:export
   #:delete-resource
   #:resource-lookup)
  ;; shaders
  (:export
   #:delete-shader-buffer
   #:make-shader-buffer
   #:update-shader-buffer)
  ;; animation states
  (:export
   #:sprite
   #:fade
   #:rotate)
  ;; collision detection
  (:export
   #:on-collision-continue
   #:on-collision-enter
   #:on-collision-exit))

(defpackage #:pyx.shader
  (:use #:shadow.glsl #:umbra.common))
