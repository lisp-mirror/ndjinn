(in-package #:cl-user)

(defpackage #:pyx.package
  (:use #:cl)
  (:export
   #:define-package))

(in-package #:pyx.package)

(defmacro define-package (package &body body)
  `(defpackage ,package
     ,@(mapcan
        (lambda (x)
          (destructuring-bind (option . items) x
            (case option
              (:inherit-from
               (destructuring-bind (from . symbols) items
                 `((:import-from ,from ,@symbols)
                   (:export ,@symbols))))
              (t `(,x)))))
        body)))

(define-package #:pyx.component
  (:local-nicknames
   (#:~ #:origin.swizzle)
   (#:a #:alexandria)
   (#:avl #:avl-tree)
   (#:m4 #:origin.mat4)
   (#:math #:origin)
   (#:q #:origin.quat)
   (#:u #:golden-utils)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4))
  (:use #:cl)
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
  ;; mouse-control
  (:export
   #:mouse-control)
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

(uiop:define-package #:pyx.shader
  (:use-reexport #:shadow.glsl #:umbra.common))

(define-package #:pyx
  (:local-nicknames
   (#:a #:alexandria)
   (#:avl #:avl-tree)
   (#:comp #:pyx.component)
   (#:ff #:filtered-functions)
   (#:font #:3b-bmfont)
   (#:glob #:global-vars)
   (#:gph #:cl-graph)
   (#:lp #:lparallel)
   (#:lpq #:lparallel.queue)
   (#:m3 #:origin.mat3)
   (#:m4 #:origin.mat4)
   (#:math #:origin)
   (#:q #:origin.quat)
   (#:ss #:split-sequence)
   (#:sv #:static-vectors)
   (#:u #:golden-utils)
   (#:v2 #:origin.vec2)
   (#:v3 #:origin.vec3)
   (#:v4 #:origin.vec4))
  (:use #:cl)
  (:inherit-from #:pyx.component
                 ;; types
                 #:camera
                 #:collider
                 #:font
                 #:geometry
                 #:id
                 #:mesh
                 #:mouse-control
                 #:node
                 #:render
                 #:sprite
                 #:transform
                 ;; transform
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
   #:define-collider-plan
   #:define-collision-hook
   #:entity-picked-p
   #:pick-entity)
  ;; config
  (:export
   #:get-config-option)
  ;; core
  (:export
   #:start-engine
   #:stop-engine)
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
   #:entity-children
   #:entity-parent
   #:has-component-p
   #:make-entity)
  ;; framebuffer
  (:export
   #:define-framebuffer)
  ;; geometry
  (:export
   #:define-geometry
   #:define-geometry-layout
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
   #:on-gamepad-analog-move
   #:on-gamepad-attach
   #:on-gamepad-detach
   #:on-gamepad-enabled)
  ;; material
  (:export
   #:as-uniform
   #:define-material
   #:set-uniforms)
  ;; prefab
  (:export
   #:define-prefab)
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
   #:delete-shader-buffer
   #:make-shader-buffer
   #:read-shader-buffer
   #:update-shader-buffer
   #:write-shader-buffer)
  ;; texture
  (:export
   #:define-texture)
  ;; viewport
  (:export
   #:define-viewport
   #:get-viewport-dimensions))
