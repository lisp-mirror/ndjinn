(in-package #:%pyx.component.sprite)

(ent:define-component sprite ()
  ((%name :reader name
          :initarg :sprite/name)
   (%asset :reader asset
           :initarg :sprite/asset)
   (%frames :reader frames
            :initarg :sprite/frames
            :initform 1)
   (%spritesheet :accessor spritesheet)
   (%index :accessor index)
   (%initial-index :accessor initial-index))
  (:sorting :after c/render:render))

;;; entity hooks

(ent:define-entity-hook :attach (entity sprite)
  (setf spritesheet (asset.sprite:make-spritesheet asset)
        index (u:href (asset.sprite:sprites spritesheet) name)
        initial-index index))

(ent:define-entity-hook :pre-render (entity sprite)
  (mat:set-uniforms (c/render:current-material entity)
                    :sprite.index index))

(ent:define-entity-hook :render (entity sprite)
  (gl:bind-vertex-array (asset.sprite:vao spritesheet))
  (gl:draw-arrays :triangle-strip 0 4)
  (gl:bind-vertex-array 0))
