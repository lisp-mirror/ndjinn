(in-package #:cl-user)

(uiop:define-package #:ndjinn.shader
  (:use-reexport
   #:net.mfiano.lisp.shadow.glsl
   #:net.mfiano.lisp.umbra.common)
  ;; functions
  (:export
   #:full-quad/vertex
   #:full-quad-no-uv/vertex)
  ;; shaders
  (:export
   #:collider
   #:default
   #:font
   #:full-quad
   #:matcap
   #:quad))

(defpackage #:ndjinn
  (:local-nicknames
   (#:avl #:net.mfiano.lisp.algae.data-structures.avl-tree)
   (#:curve #:net.mfiano.lisp.algae.spline.cubic-bezier)
   (#:dll #:net.mfiano.lisp.algae.data-structures.doubly-linked-list)
   (#:ff #:filtered-functions)
   (#:font #:3b-bmfont)
   (#:glob #:global-vars)
   (#:log #:verbose)
   (#:lp #:lparallel)
   (#:lpq #:lparallel.queue)
   (#:m3 #:net.mfiano.lisp.origin.mat3)
   (#:m4 #:net.mfiano.lisp.origin.mat4)
   (#:math #:net.mfiano.lisp.origin)
   (#:q #:net.mfiano.lisp.origin.quat)
   (#:shadow #:net.mfiano.lisp.shadow)
   (#:shader #:ndjinn.shader)
   (#:umbra.sprite #:net.mfiano.lisp.umbra.sprite)
   (#:ss #:split-sequence)
   (#:sv #:static-vectors)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:uuid #:net.mfiano.lisp.algae.uuid)
   (#:v2 #:net.mfiano.lisp.origin.vec2)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4))
  (:use #:cl)
  (:shadow #:debug)
  (:export
   ;; types
   #:camera
   #:collider
   #:curve
   #:debug
   #:font
   #:geometry
   #:id
   #:mesh
   #:mouse-control
   #:node
   #:render
   #:sprite
   #:transform)
  ;; live support
  (:export
   #:send-to-repl)
  ;; animate
  (:export
   #:define-animate-hook
   #:make-animation)
  ;; transform
  (:export
   #:clamp-translation
   #:get-rotation
   #:get-scale
   #:get-translation
   #:resolve-normal-matrix
   #:rotate-entity
   #:rotate-entity/velocity
   #:scale-entity
   #:scale-entity/velocity
   #:transform-direction
   #:transform-point
   #:transform-vector
   #:translate-entity
   #:translate-entity/velocity)
  ;; camera
  (:export
   #:get-camera-zoom
   #:get-current-camera
   #:zoom-camera)
  ;; curve
  (:export
   #:evaluate-curve
   #:rescale-curve)
  ;; id
  (:export
   #:get-display-id
   #:get-uuid)
  ;; font
  (:export
   #:get-font-dimensions
   #:get-font-position
   #:get-font-offset)
  ;; asset
  (:export
   #:define-asset-pool
   #:delete-asset
   #:find-asset
   #:resolve-path
   #:with-asset-cache)
  ;; clock
  (:export
   #:get-fps
   #:get-frame-count
   #:get-frame-time
   #:get-running-time)
  ;; collision detection
  (:export
   #:define-collision-plan
   #:define-collision-hook
   #:entity-picked-p
   #:pick-entity
   #:unpick-entity)
  ;; collider shapes
  (:export
   #:box
   #:sphere)
  ;; context
  (:export
   #:define-context
   #:on-context-create
   #:on-context-destroy
   #:user-data)
  ;; core
  (:export
   #:pause-game
   #:start-engine
   #:stop-engine
   #:toggle-pause
   #:unpause-game)
  ;; curve
  (:export
   #:define-curve)
  ;; deploy
  (:export
   #:deploy)
  ;; display
  (:export
   #:window-position
   #:window-size
   #:window-title)
  ;; entity
  (:export
   #:attach-component
   #:define-component
   #:define-entity-hook
   #:define-entity-query
   #:define-entity-query-types
   #:delete-entity
   #:detach-component
   #:detach-components
   #:do-nodes
   #:entity-children
   #:entity-parent
   #:find-parent
   #:get-entity-count
   #:has-component-p
   #:make-entity)
  ;; framebuffer
  (:export
   #:define-framebuffer)
  ;; geometry
  (:export
   #:define-geometry
   #:define-geometry-layout
   #:geometry/dirty
   #:geometry/instances
   #:update-geometry)
  ;; hardware
  (:export
   #:get-hardware-info)
  ;; input
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
   #:on-gamepad-attach
   #:on-gamepad-detach
   #:on-gamepad-enabled
   #:on-window-event-enabled
   #:on-window-event-enter
   #:on-window-event-exit)
  ;; material
  (:export
   #:as-uniform
   #:define-material
   #:set-uniforms)
  ;; node
  (:export
   #:disable-entity
   #:enable-entity
   #:entity-enabled-p
   #:entity-paused-p
   #:get-root-node
   #:reparent-node)
  ;; prefab
  (:export
   #:define-prefab
   #:load-prefab)
  ;; render pass
  (:export
   #:define-render-pass)
  ;; scene
  (:export
   #:define-scene
   #:get-registered-scene-names
   #:get-scene-name
   #:switch-scene)
  ;; shader
  (:export
   #:bind-shader-buffer
   #:clear-shader-buffer
   #:delete-shader-buffer
   #:make-shader-buffer
   #:read-shader-buffer
   #:unbind-shader-buffer
   #:update-shader-buffer
   #:with-shader-buffers
   #:write-shader-buffer)
  ;; texture
  (:export
   #:define-texture)
  ;; viewport
  (:export
   #:define-viewport
   #:get-viewport-size)
  ;; sprite
  (:export
   #:find-sprite
   #:make-spritesheet
   #:spritesheet-vao))
