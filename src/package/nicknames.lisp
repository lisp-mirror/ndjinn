(in-package #:%pyx.package)

(define-local-nicknames #:%pyx.animation
  (#:a #:alexandria)
  (#:comp #:%pyx.component)
  (#:clock #:%pyx.clock)
  (#:comp #:%pyx.component)
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
  (#:mesh #:%pyx.mesh)
  (#:meta #:%pyx.metadata)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.asset.spritesheet
  (#:asset #:%pyx.asset)
  (#:ent #:%pyx.entity)
  (#:comp #:%pyx.component)
  (#:shader #:%pyx.shader)
  (#:u #:golden-utils)
  (#:v2 #:origin.vec2))

(define-local-nicknames #:%pyx.clock
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:ctx #:%pyx.context)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.collision-detection
  (#:a #:alexandria)
  (#:comp #:%pyx.component)
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
  (#:util #:%pyx.util)
  (#:v3 #:origin.vec3)
  (#:v4 #:origin.vec4)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.component
  (#:~ #:origin.swizzle)
  (#:a #:alexandria)
  (#:anim #:%pyx.animation)
  (#:asset #:%pyx.asset)
  (#:asset.sprite #:%pyx.asset.spritesheet)
  (#:cd #:%pyx.collision-detection)
  (#:cfg #:%pyx.config)
  (#:clock #:%pyx.clock)
  (#:ctx #:%pyx.context)
  (#:dll #:doubly-linked-list)
  (#:ent #:%pyx.entity)
  (#:fb #:%pyx.framebuffer)
  (#:geom #:%pyx.geometry)
  (#:in #:%pyx.input)
  (#:m4 #:origin.mat4)
  (#:mat #:%pyx.material)
  (#:math #:origin)
  (#:mesh #:%pyx.mesh)
  (#:meta #:%pyx.metadata)
  (#:ogl #:%pyx.opengl)
  (#:prefab #:%pyx.prefab)
  (#:q #:origin.quat)
  (#:render #:%pyx.render)
  (#:scene #:%pyx.scene)
  (#:tex #:%pyx.texture)
  (#:tfm #:%pyx.transform)
  (#:u #:golden-utils)
  (#:ui.font #:%pyx.ui.font)
  (#:util #:%pyx.util)
  (#:v2 #:origin.vec2)
  (#:v3 #:origin.vec3)
  (#:v4 #:origin.vec4)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.config
  (#:a #:alexandria)
  (#:glob #:global-vars)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.context
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.core
  (#:cd #:%pyx.collision-detection)
  (#:cfg #:%pyx.config)
  (#:clock #:%pyx.clock)
  (#:comp #:%pyx.component)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:display #:%pyx.display)
  (#:hw #:%pyx.hardware)
  (#:in #:%pyx.input)
  (#:scene #:%pyx.scene)
  (#:shader #:%pyx.shader)
  (#:u #:golden-utils)
  (#:util #:%pyx.util))

(define-local-nicknames #:%pyx.display
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:clock #:%pyx.clock)
  (#:comp #:%pyx.component)
  (#:ctx #:%pyx.context)
  (#:ogl #:%pyx.opengl)
  (#:v2 #:origin.vec2))

(define-local-nicknames #:%pyx.entity
  (#:a #:alexandria)
  (#:comp #:%pyx.component)
  (#:ff #:filtered-functions)
  (#:glob #:global-vars)
  (#:gph #:cl-graph)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.framebuffer
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:ctx #:%pyx.context)
  (#:meta #:%pyx.metadata)
  (#:ogl #:%pyx.opengl)
  (#:tex #:%pyx.texture)
  (#:u #:golden-utils)
  (#:util #:%pyx.util))

(define-local-nicknames #:%pyx.geometry
  (#:a #:alexandria)
  (#:meta #:%pyx.metadata)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.hardware
  (#:a #:alexandria)
  (#:glob #:global-vars))

(define-local-nicknames #:%pyx.image
  (#:a #:alexandria)
  (#:asset #:%pyx.asset)
  (#:ss #:split-sequence)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.input
  (#:a #:alexandria)
  (#:asset #:%pyx.asset)
  (#:cfg #:%pyx.config)
  (#:ctx #:%pyx.context)
  (#:u #:golden-utils)
  (#:v2 #:origin.vec2))

(define-local-nicknames #:%pyx.material
  (#:a #:alexandria)
  (#:comp #:%pyx.component)
  (#:ctx #:%pyx.context)
  (#:fb #:%pyx.framebuffer)
  (#:lp #:lparallel)
  (#:meta #:%pyx.metadata)
  (#:ogl #:%pyx.opengl)
  (#:render #:%pyx.render)
  (#:scene #:%pyx.scene)
  (#:tex #:%pyx.texture)
  (#:u #:golden-utils)
  (#:util #:%pyx.util))

(define-local-nicknames #:%pyx.mesh
  (#:a #:alexandria)
  (#:ctx #:%pyx.context)
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
  (#:comp #:%pyx.component)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:meta #:%pyx.metadata)
  (#:render #:%pyx.render)
  (#:scene #:%pyx.scene)
  (#:u #:golden-utils)
  (#:util #:%pyx.util)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.render
  (#:a #:alexandria)
  (#:comp #:%pyx.component)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:fb #:%pyx.framebuffer)
  (#:mat #:%pyx.material)
  (#:meta #:%pyx.metadata)
  (#:ogl #:%pyx.opengl)
  (#:scene #:%pyx.scene)
  (#:u #:golden-utils)
  (#:util #:%pyx.util)
  (#:v4 #:origin.vec4)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.scene
  (#:a #:alexandria)
  (#:cd #:%pyx.collision-detection)
  (#:comp #:%pyx.component)
  (#:ctx #:%pyx.context)
  (#:ent #:%pyx.entity)
  (#:meta #:%pyx.metadata)
  (#:render #:%pyx.render)
  (#:prefab #:%pyx.prefab)
  (#:u #:golden-utils)
  (#:util #:%pyx.util)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.shader
  (#:a #:alexandria)
  (#:ctx #:%pyx.context)
  (#:hw #:%pyx.hardware)
  (#:u #:golden-utils)
  (#:util #:%pyx.util))

(define-local-nicknames #:%pyx.texture
  (#:a #:alexandria)
  (#:asset #:%pyx.asset)
  (#:ctx #:%pyx.context)
  (#:img #:%pyx.image)
  (#:lp #:lparallel)
  (#:meta #:%pyx.metadata)
  (#:u #:golden-utils)
  (#:util #:%pyx.util)
  (#:v4 #:origin.vec4))

(define-local-nicknames #:%pyx.transform
  (#:math #:origin)
  (#:q #:origin.quat)
  (#:v3 #:origin.vec3))

(define-local-nicknames #:%pyx.ui.font
  (#:comp #:%pyx.component)
  (#:font #:3b-bmfont)
  (#:u #:golden-utils)
  (#:v2 #:origin.vec2)
  (#:vp #:%pyx.viewport))

(define-local-nicknames #:%pyx.util
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:glob #:global-vars)
  (#:hw #:%pyx.hardware)
  (#:lp #:lparallel)
  (#:q #:lparallel.queue)
  (#:u #:golden-utils))

(define-local-nicknames #:%pyx.viewport
  (#:a #:alexandria)
  (#:cfg #:%pyx.config)
  (#:comp #:%pyx.component)
  (#:ctx #:%pyx.context)
  (#:meta #:%pyx.metadata)
  (#:scene #:%pyx.scene)
  (#:u #:golden-utils)
  (#:util #:%pyx.util)
  (#:v2 #:origin.vec2))
