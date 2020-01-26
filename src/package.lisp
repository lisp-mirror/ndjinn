(in-package #:cl-user)

(defpackage #:pyx
  (:local-nicknames
   (#:~ #:origin.swizzle)
   (#:a #:alexandria)
   (#:ff #:filtered-functions)
   (#:font #:3b-bmfont)
   (#:font-spec #:3b-bmfont-json)
   (#:log #:verbose)
   (#:math #:origin)
   (#:m3 #:origin.mat3)
   (#:m4 #:origin.mat4)
   (#:q #:origin.quat)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4)
   (#:u #:golden-utils))
  (:use #:cl)
  ;; engine
  (:export
   #:start-engine
   #:stop-engine
   #:deploy)
  ;; common
  (:export
   #:cfg
   #:meta
   #:get-window-resolution
   #:get-total-time
   #:get-viewport-dimensions)
  ;; hardware
  (:export
   #:get-cpu
   #:get-gpu-vendor
   #:get-gpu-device
   #:get-gpu-make/model
   #:get-gpu-version)
  ;; scenes
  (:export
   #:get-scene
   #:get-scene-name
   #:load-scene
   #:switch-scene)
  ;; definitions
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
   #:define-texture
   #:define-view)
  ;; input
  (:export
   #:get-gamepad-analog
   #:get-mouse-position
   #:get-mouse-scroll
   #:on-button-enabled
   #:on-button-enter
   #:on-button-exit
   #:on-gamepad-analog-move
   #:on-gamepad-attach
   #:on-gamepad-enabled
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
   #:font
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
   #:set-uniforms
   #:update-shader-buffer)
  ;; animation states
  (:export
   #:sprite
   #:fade
   #:rotate)
  ;; collision detection
  (:export
   #:collider/layer
   #:define-collision-hook)
  ;; transform protocol
  (:export
   #:rotate-entity
   #:rotate-entity/velocity
   #:scale-entity
   #:scale-entity/velocity
   #:transform-direction
   #:transform-point
   #:transform-vector
   #:translate-entity
   #:translate-entity/velocity)
  ;; definitions
  (:export
   #:debug
   #:mesh
   #:full-quad
   #:quad))

(defpackage #:pyx.shader
  (:use #:shadow.glsl #:umbra.common))
