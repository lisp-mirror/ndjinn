(in-package #:%pyx.package)

(define-local-nicknames #:%pyx.animation
  (#:a #:alexandria)
  (#:c/anim #:%pyx.component.animate)
  (#:c/render #:%pyx.component.render)
  (#:c/sprite #:%pyx.component.sprite)
  (#:clock #:%pyx.clock)
  (#:dll #:doubly-linked-list)
  (#:ff #:filtered-functions)
  (#:mat #:%pyx.material)
  (#:math #:origin)
  (#:meta #:%pyx.metadata)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.asset
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:ctx #:%pyx.context)
  (#:mesh #:%pyx.asset.mesh)
  (#:meta #:%pyx.metadata)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.asset.image
  (#:a #:alexandria)
  (#:asset #:%pyx.asset)
  (#:hdr #:%pyx.asset.image.hdr)
  (#:png #:%pyx.asset.image.png))

(define-local-nicknames #:%pyx.asset.image.hdr
  (#:a #:alexandria)
  (#:img #:%pyx.asset.image)
  (#:ss #:split-sequence)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.asset.image.png
  (#:a #:alexandria)
  (#:img #:%pyx.asset.image))

(define-local-nicknames #:%pyx.asset.mesh
  (#:a #:alexandria)
  (#:ctx #:%pyx.context)
  (#:parse #:%pyx.binary-parser)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.asset.spritesheet
  (#:asset #:%pyx.asset)
  (#:ent #:%pyx.entity)
  (#:c/render #:%pyx.component.render)
  (#:shader #:%pyx.shader)
  (#:u #:golden-utils)
  (#:v2 #:origin.vec2))

(define-local-nicknames #:%pyx.avl-tree
  (#:a #:alexandria)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.clock
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:ctx #:%pyx.context)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.collision-detection
  (#:a #:alexandria)
  (#:avl #:%pyx.avl-tree)
  (#:c/camera #:%pyx.component.camera)
  (#:c/collider #:%pyx.component.collider)
  (#:c/transform #:%pyx.component.transform)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:in #:%pyx.input)
  (#:m3 #:origin.mat3)
  (#:m4 #:origin.mat4)
  (#:mat #:%pyx.material)
  (#:math #:origin)
  (#:meta #:%pyx.metadata)
  (#:scene #:%pyx.scene)
  (#:u #:golden-utils)
  (#:v3 #:origin.vec3)
  (#:v4 #:origin.vec4)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.component.animate
  (#:a #:alexandria)
  (#:anim #:%pyx.animation)
  (#:c/mesh #:%pyx.component.mesh)
  (#:c/render #:%pyx.component.render)
  (#:c/sprite #:%pyx.component.sprite)
  (#:dll #:doubly-linked-list)
  (#:ent #:%pyx.entity)
  (#:meta #:%pyx.metadata)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.component.camera
  (#:a #:alexandria)
  (#:c/render #:%pyx.component.render)
  (#:c/transform #:%pyx.component.transform)
  (#:ent #:%pyx.entity)
  (#:in #:%pyx.input)
  (#:m4 #:origin.mat4)
  (#:math #:origin)
  (#:q #:origin.quat)
  (#:tfm #:%pyx.transform)
  (#:u #:golden-utils)
  (#:v3 #:origin.vec3)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.component.collider
  (#:c/mesh #:%pyx.component.mesh)
  (#:c/render #:%pyx.component.render)
  (#:cd #:%pyx.collision-detection)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:mat #:%pyx.material)
  (#:scene #:%pyx.scene)
  (#:v4 #:origin.vec4))

(define-local-nicknames #:%pyx.component.font
  (#:asset #:%pyx.asset)
  (#:c/geom #:%pyx.component.geometry)
  (#:c/render #:%pyx.component.render)
  (#:c/transform #:%pyx.component.transform)
  (#:clock #:%pyx.clock)
  (#:ent #:%pyx.entity)
  (#:geom #:%pyx.geometry)
  (#:tex #:%pyx.texture)
  (#:u #:golden-utils)
  (#:ui.font #:%pyx.ui.font)
  (#:v2 #:origin.vec2)
  (#:v3 #:origin.vec3))

(define-local-nicknames #:%pyx.component.geometry
  (#:c/render #:%pyx.component.render)
  (#:ent #:%pyx.entity)
  (#:geom #:%pyx.geometry))

(define-local-nicknames #:%pyx.component.id
  (#:a #:alexandria)
  (#:c/node #:%pyx.component.node)
  (#:cd #:%pyx.collision-detection)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:scene #:%pyx.scene)
  (#:u #:golden-utils)
  (#:uuid #:%pyx.uuid))

(define-local-nicknames #:%pyx.component.mesh
  (#:asset #:%pyx.asset)
  (#:c/render #:%pyx.component.render)
  (#:ent #:%pyx.entity)
  (#:mesh #:%pyx.asset.mesh)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.component.node
  (#:a #:alexandria)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:prefab #:%pyx.prefab)
  (#:scene #:%pyx.scene))

(define-local-nicknames #:%pyx.component.render
  (#:a #:alexandria)
  (#:avl #:%pyx.avl-tree)
  (#:c/camera #:%pyx.component.camera)
  (#:c/sprite #:%pyx.component.sprite)
  (#:c/transform #:%pyx.component.transform)
  (#:c/id #:%pyx.component.id)
  (#:ctx #:%pyx.context)
  (#:fb #:%pyx.framebuffer)
  (#:ent #:%pyx.entity)
  (#:mat #:%pyx.material)
  (#:ogl #:%pyx.opengl)
  (#:render #:%pyx.render)
  (#:scene #:%pyx.scene)
  (#:u #:golden-utils)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.component.sprite
  (#:asset #:%pyx.asset)
  (#:asset.sprite #:%pyx.asset.spritesheet)
  (#:c/render #:%pyx.component.render)
  (#:ent #:%pyx.entity)
  (#:mat #:%pyx.material)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.component.transform
  (#:~ #:origin.swizzle)
  (#:a #:alexandria)
  (#:c/camera #:%pyx.component.camera)
  (#:c/node #:%pyx.component.node)
  (#:c/render #:%pyx.component.render)
  (#:cfg #:%pyx.config)
  (#:clock #:%pyx.clock)
  (#:ent #:%pyx.entity)
  (#:m4 #:origin.mat4)
  (#:mat #:%pyx.material)
  (#:math #:origin)
  (#:q #:origin.quat)
  (#:tfm #:%pyx.transform)
  (#:v3 #:origin.vec3)
  (#:v4 #:origin.vec4))

(define-local-nicknames #:%pyx.config
  (#:a #:alexandria)
  (#:glob #:global-vars)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.context
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.core
  (#:c/node #:%pyx.component.node)
  (#:c/transform #:%pyx.component.transform)
  (#:cd #:%pyx.collision-detection)
  (#:cfg #:%pyx.config)
  (#:clock #:%pyx.clock)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:display #:%pyx.display)
  (#:hw #:%pyx.hardware)
  (#:in #:%pyx.input)
  (#:live #:%pyx.live-support)
  (#:scene #:%pyx.scene)
  (#:shader #:%pyx.shader)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils)
  (#:util #:%pyx.util))

(define-local-nicknames #:%pyx.display
  (#:a #:alexandria)
  (#:c/render #:%pyx.component.render)
  (#:cfg #:%pyx.config)
  (#:clock #:%pyx.clock)
  (#:ctx #:%pyx.context)
  (#:ogl #:%pyx.opengl)
  (#:v2 #:origin.vec2))

(define-local-nicknames #:%pyx.entity
  (#:a #:alexandria)
  (#:c/node #:%pyx.component.node)
  (#:ff #:filtered-functions)
  (#:glob #:global-vars)
  (#:gph #:cl-graph)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.framebuffer
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:ctx #:%pyx.context)
  (#:live #:%pyx.live-support)
  (#:meta #:%pyx.metadata)
  (#:ogl #:%pyx.opengl)
  (#:tex #:%pyx.texture)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.geometry
  (#:a #:alexandria)
  (#:meta #:%pyx.metadata)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.hardware
  (#:a #:alexandria)
  (#:glob #:global-vars))

(define-local-nicknames #:%pyx.input
  (#:a #:alexandria)
  (#:asset #:%pyx.asset)
  (#:cfg #:%pyx.config)
  (#:ctx #:%pyx.context)
  (#:u #:golden-utils)
  (#:v2 #:origin.vec2))

(define-local-nicknames #:%pyx.live-support
  (#:a #:alexandria)
  (#:tp #:%pyx.thread-pool))

(define-local-nicknames #:%pyx.material
  (#:a #:alexandria)
  (#:c/render #:%pyx.component.render)
  (#:ctx #:%pyx.context)
  (#:fb #:%pyx.framebuffer)
  (#:live #:%pyx.live-support)
  (#:lp #:lparallel)
  (#:meta #:%pyx.metadata)
  (#:ogl #:%pyx.opengl)
  (#:render #:%pyx.render)
  (#:scene #:%pyx.scene)
  (#:tex #:%pyx.texture)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils)
  (#:util #:%pyx.util))

(define-local-nicknames #:%pyx.metadata
  (#:glob #:global-vars)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.opengl
  (#:a #:alexandria)
  (#:glob #:global-vars))

(define-local-nicknames #:%pyx.prefab
  (#:a #:alexandria)
  (#:c/id #:%pyx.component.id)
  (#:c/node #:%pyx.component.node)
  (#:c/render #:%pyx.component.render)
  (#:ctx #:%pyx.context)
  (#:live #:%pyx.live-support)
  (#:ent #:%pyx.entity)
  (#:meta #:%pyx.metadata)
  (#:render #:%pyx.render)
  (#:scene #:%pyx.scene)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils)
  (#:util #:%pyx.util)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.render
  (#:a #:alexandria)
  (#:avl #:%pyx.avl-tree)
  (#:c/render #:%pyx.component.render)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:fb #:%pyx.framebuffer)
  (#:live #:%pyx.live-support)
  (#:mat #:%pyx.material)
  (#:meta #:%pyx.metadata)
  (#:ogl #:%pyx.opengl)
  (#:scene #:%pyx.scene)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils)
  (#:v4 #:origin.vec4)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.scene
  (#:a #:alexandria)
  (#:c/node #:%pyx.component.node)
  (#:cd #:%pyx.collision-detection)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:live #:%pyx.live-support)
  (#:meta #:%pyx.metadata)
  (#:render #:%pyx.render)
  (#:prefab #:%pyx.prefab)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.shader
  (#:a #:alexandria)
  (#:ctx #:%pyx.context)
  (#:hw #:%pyx.hardware)
  (#:live #:%pyx.live-support)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.texture
  (#:a #:alexandria)
  (#:asset #:%pyx.asset)
  (#:ctx #:%pyx.context)
  (#:img #:%pyx.asset.image)
  (#:live #:%pyx.live-support)
  (#:lp #:lparallel)
  (#:meta #:%pyx.metadata)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils)
  (#:v4 #:origin.vec4))

(define-local-nicknames #:%pyx.thread-pool
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:glob #:global-vars)
  (#:hw #:%pyx.hardware)
  (#:lp #:lparallel)
  (#:q #:lparallel.queue)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.transform
  (#:math #:origin)
  (#:q #:origin.quat)
  (#:v3 #:origin.vec3))

(define-local-nicknames #:%pyx.ui.font
  (#:c/font #:%pyx.component.font)
  (#:font #:3b-bmfont)
  (#:u #:golden-utils)
  (#:v2 #:origin.vec2)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.util
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.uuid
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.viewport
  (#:a #:alexandria)
  (#:c/id #:%pyx.component.id)
  (#:cfg #:%pyx.config)
  (#:ctx #:%pyx.context)
  (#:live #:%pyx.live-support)
  (#:meta #:%pyx.metadata)
  (#:scene #:%pyx.scene)
  (#:tp #:%pyx.thread-pool)
  (#:u #:golden-utils)
  (#:v2 #:origin.vec2))
