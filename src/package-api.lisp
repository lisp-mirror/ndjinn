(in-package #:%pyx.package)

(define-package #:pyx
  (:use #:cl)
  (:inherit-from #:%pyx.animation
                 #:define-animation-sequence)
  (:inherit-from #:%pyx.asset
                 #:define-asset-pool)
  (:inherit-from #:%pyx.clock
                 #:get-fps
                 #:get-frame-count
                 #:get-frame-time
                 #:get-running-time)
  (:inherit-from #:%pyx.collision-detection
                 #:define-collider-plan
                 #:define-collision-hook
                 #:pick-entity)
  (:inherit-from #:%pyx.component.animate
                 #:animate)
  (:inherit-from #:%pyx.component.camera
                 #:camera)
  (:inherit-from #:%pyx.component.collider
                 #:collider)
  (:inherit-from #:%pyx.component.font
                 #:font)
  (:inherit-from #:%pyx.component.id
                 #:id)
  (:inherit-from #:%pyx.component.mesh
                 #:mesh)
  (:inherit-from #:%pyx.component.node
                 #:node)
  (:inherit-from #:%pyx.component.render
                 #:current-material
                 #:render)
  (:inherit-from #:%pyx.component.sprite
                 #:sprite)
  (:inherit-from #:%pyx.component.transform
                 #:get-rotation
                 #:get-scale
                 #:get-translation
                 #:resolve-normal-matrix
                 #:rotate-entity
                 #:rotate-entity/velocity
                 #:scale-entity
                 #:scale-entity/velocity
                 #:transform
                 #:transform-direction
                 #:transform-point
                 #:transform-vector
                 #:translate-entity
                 #:translate-entity/velocity)
  (:inherit-from #:%pyx.config
                 #:get-config-option)
  (:inherit-from #:%pyx.core
                 #:start-engine
                 #:stop-engine)
  (:inherit-from #:%pyx.entity
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
                 #:make-entity)
  (:inherit-from #:%pyx.framebuffer
                 #:define-framebuffer)
  (:inherit-from #:%pyx.geometry
                 #:define-geometry
                 #:define-geometry-layout
                 #:delete-geometry
                 #:draw-geometry
                 #:make-geometry
                 #:update-geometry)
  (:inherit-from #:%pyx.hardware
                 #:get-hardware-info)
  (:inherit-from #:%pyx.input
                 #:get-mouse-position
                 #:on-button-enabled
                 #:on-button-enter
                 #:on-button-exit)
  (:inherit-from #:%pyx.material
                 #:as-uniform
                 #:define-material
                 #:set-uniforms)
  (:inherit-from #:%pyx.prefab
                 #:define-prefab)
  (:inherit-from #:%pyx.render
                 #:define-render-pass)
  (:inherit-from #:%pyx.asset
                 #:delete-asset
                 #:find-asset
                 #:resolve-path
                 #:with-asset-cache)
  (:inherit-from #:%pyx.scene
                 #:define-scene
                 #:get-registered-scene-names
                 #:get-scene-name
                 #:switch-scene)
  (:inherit-from #:%pyx.shader
                 #:delete-shader-buffer
                 #:make-shader-buffer
                 #:read-shader-buffer
                 #:update-shader-buffer
                 #:write-shader-buffer)
  (:inherit-from #:%pyx.texture
                 #:define-texture)
  (:inherit-from #:%pyx.viewport
                 #:define-viewport
                 #:get-viewport-dimensions))

(uiop:define-package #:pyx.shader
  (:use-reexport #:shadow.glsl #:umbra.common))
