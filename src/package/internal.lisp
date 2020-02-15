(in-package #:%pyx.package)

(define-package #:%pyx.animation
  (:use #:cl)
  (:export
   #:process-animation-states)
  ;; public
  (:export
   #:define-animation-sequence
   #:opacity
   #:sprite))

(define-package #:%pyx.asset
  (:use #:cl)
  ;; public
  (:export
   #:resolve-system-path)
  (:export
   #:define-asset-pool
   #:delete-asset
   #:find-asset
   #:resolve-path
   #:with-asset-cache))

(define-package #:%pyx.asset.spritesheet
  (:use #:cl)
  (:export
   #:make-spritesheet
   #:sprites
   #:vao))

(define-package #:%pyx.clock
  (:use #:cl)
  (:export
   #:debug-time-p
   #:frame-count
   #:get-alpha
   #:make-clock
   #:tick)
  ;; public
  (:export
   #:get-fps
   #:get-frame-count
   #:get-frame-time
   #:get-running-time))

(define-package #:%pyx.collision-detection
  (:use #:cl)
  (:export
   #:%on-collision-continue
   #:%on-collision-enter
   #:%on-collision-exit
   #:callback-entities
   #:compute-collisions
   #:deregister-collider
   #:get-collision-targets
   #:make-collision-system
   #:make-picking-ray
   #:make-shape
   #:on-collision-continue
   #:on-collision-enter
   #:on-collision-exit
   #:register-collider
   #:update-shape)
  ;; public
  (:export
   #:define-collider-plan
   #:define-collision-hook
   #:pick-entity))

(define-package #:%pyx.component
  (:use #:cl)
  ;; animate
  (:export
   #:animate)
  ;; camera
  (:export
   #:camera)
  ;; collider
  (:export
   #:collider)
  ;; font
  (:export
   #:font)
  ;; geometry
  (:export
   #:geometry)
  ;; id
  (:export
   #:id)
  ;; mesh
  (:export
   #:mesh)
  ;; node
  (:export
   #:node
   #:node/children
   #:node/parent)
  ;; render
  (:export
   #:render)
  ;; sprite
  (:export
   #:sprite)
  ;; transform
  (:export
   #:get-rotation
   #:get-scale
   #:get-translation
   #:resolve-normal-matrix
   #:rotate-entity
   #:rotate-entity/velocity
   #:scale-entity
   #:scale-entity/velocity
   #:transform
   #:transform-node
   #:transform-direction
   #:transform-point
   #:transform-vector
   #:translate-entity
   #:translate-entity/velocity))

(define-package #:%pyx.config
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:=allow-screensaver=
   #:=debug=
   #:=debug-interval=
   #:=delta-time=
   #:=release=
   #:=threads=
   #:=title=
   #:=vsync=
   #:=window-width=
   #:=window-height=
   #:load)
  ;; public
  (:export
   #:get-config-option))

(define-package #:%pyx.context
  (:use #:cl)
  (:export
   #:*context*
   #:assets
   #:clock
   #:current-scene
   #:display
   #:framebuffers
   #:input-data
   #:make-context
   #:running-p
   #:scenes
   #:shaders))

(define-package #:%pyx.core
  (:use #:cl)
  ;; public
  (:export
   #:start-engine
   #:stop-engine))

(define-package #:%pyx.display
  (:use #:cl)
  (:export
   #:kill
   #:make-display
   #:refresh-rate
   #:render))

(define-package #:%pyx.entity
  (:use #:cl)
  (:export
   #:=component-order=
   #:compute-component-initargs
   #:compute-component-order
   #:get-slots
   #:make-class
   #:mixin
   #:on-attach
   #:on-create
   #:on-delete
   #:on-detach
   #:on-physics-update
   #:on-pre-render
   #:on-render
   #:on-update
   #:query
   #:register)
  ;; public
  (:export
   #:attach-component
   #:define-component
   #:define-entity-hook
   #:define-entity-query
   #:define-entity-query-types
   #:delete-entity
   #:detach-component
   #:detach-components
   #:entity-children
   #:entity-parent
   #:has-component-p
   #:make-entity))

(define-package #:%pyx.framebuffer
  (:use #:cl)
  (:shadow
   #:find
   #:load)
  (:export
   #:attachment-names->points
   #:find
   #:find-spec
   #:find-attachment-spec
   #:get-attachment-texture
   #:load
   #:materials
   #:with-framebuffer)
  ;; public
  (:export
   #:define-framebuffer))

(define-package #:%pyx.geometry
  (:use #:cl)
  ;; public
  (:export
   #:define-geometry
   #:define-geometry-layout
   #:delete-geometry
   #:draw-geometry
   #:make-geometry
   #:update-geometry))

(define-package #:%pyx.hardware
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:=cpu=
   #:=cpu-count=
   #:=gpu-vendor=
   #:=gpu-device=
   #:=gpu-make/model=
   #:=gpu-version=
   #:=max-texture-size=
   #:=max-ssbo-bindings=
   #:load)
  ;; public
  (:export
   #:get-hardware-info))

(define-package #:%pyx.image
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:data
   #:height
   #:image
   #:internal-format
   #:load
   #:make-image
   #:pixel-format
   #:pixel-type
   #:width))

(define-package #:%pyx.input
  (:use #:cl)
  (:export
   #:handle-events
   #:input-data
   #:make-input-data
   #:prepare-gamepads
   #:shutdown-gamepads)
  ;; public
  (:export
   #:disable-relative-motion
   #:enable-relative-motion
   #:get-gamepad-analog
   #:get-mouse-position
   #:get-mouse-scroll
   #:mouse-motion-relative-p
   #:on-button-enabled
   #:on-button-enter
   #:on-button-exit
   #:on-gamepad-analog-move
   #:on-gamepad-attach
   #:on-gamepad-detach
   #:on-gamepad-enabled))

(define-package #:%pyx.material
  (:use #:cl)
  (:export
   #:as-uniform
   #:attachments
   #:framebuffer
   #:make-material
   #:pass
   #:register
   #:render-func
   #:resolve-uniform-func
   #:shader
   #:spec
   #:texture-unit-state
   #:uniforms)
  ;; public
  (:export
   #:define-material
   #:set-uniforms))

(define-package #:%pyx.mesh
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:draw-func
   #:load
   #:meshes
   #:primitives))

(define-package #:%pyx.metadata
  (:use #:cl)
  (:export
   #:=animation-states=
   #:=animation-sequences=
   #:=asset-pools=
   #:=collider-plans=
   #:=framebuffers=
   #:=geometry=
   #:=geometry-layouts=
   #:=materials=
   #:=render-passes=
   #:=prefabs=
   #:=scenes=
   #:=textures=
   #:=viewports=))

(define-package #:%pyx.opengl
  (:use #:cl)
  (:export
   #:+blend-mode+
   #:+depth-mode+
   #:+disabled+
   #:+enabled+
   #:+polygon-mode+
   #:named-framebuffer-draw-buffers
   #:with-debug-group))

(define-package #:%pyx.prefab
  (:use #:cl)
  (:export
   #:define-prefab
   #:deregister-prefab-entity
   #:load-prefab)
  ;; public
  (:export
   #:define-prefab))

(define-package #:%pyx.render
  (:use #:cl)
  (:export
   #:clear-pass
   #:deregister-order
   #:find-pass-spec
   #:make-order-tree
   #:name
   #:pass-order
   #:register-order)
  ;; public
  (:export
   #:define-render-pass))

(define-package #:%pyx.scene
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:collision-system
   #:draw-order
   #:load
   #:materials
   #:node-tree
   #:passes
   #:prefabs
   #:spec
   #:uuids
   #:viewports)
  ;; public
  (:export
   #:define-scene
   #:get-registered-scene-names
   #:get-scene-name
   #:switch-scene))

(define-package #:%pyx.shader
  (:use #:cl)
  (:export
   #:initialize)
  ;; public
  (:export
   #:delete-shader-buffer
   #:make-shader-buffer
   #:read-shader-buffer
   #:update-shader-buffer
   #:write-shader-buffer))

(define-package #:%pyx.texture
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:bind
   #:find-spec
   #:id
   #:load)
  ;; public
  (:export
   #:define-texture))

(define-package #:%pyx.transform
  (:use #:cl)
  (:export
   #:current
   #:incremental
   #:initialize-rotation
   #:initialize-scale
   #:initialize-translation
   #:interpolate-quaternion
   #:interpolate-vector
   #:interpolated
   #:make-scale-state
   #:make-rotate-state
   #:make-translate-state
   #:previous
   #:transform-node/quaternion
   #:transform-node/vector))

(define-package #:%pyx.ui.font
  (:use #:cl)
  (:export
   #:calculate-position
   #:map-glyphs))

(define-package #:%pyx.util
  (:use #:cl))

(define-package #:%pyx.viewport
  (:use #:cl)
  (:export
   #:active
   #:camera
   #:configure
   #:default
   #:draw-order
   #:get-by-coordinates
   #:get-entity-viewports
   #:get-manager
   #:height
   #:make-manager
   #:make-viewport
   #:picking-ray
   #:table
   #:width
   #:x
   #:y)
  ;; public
  (:export
   #:define-viewport
   #:get-viewport-dimensions))
