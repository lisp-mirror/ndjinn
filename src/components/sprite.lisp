(in-package #:%pyx.component)

(ent:define-component sprite ()
  ((%sprite/name :reader sprite/name
                 :initarg :sprite/name)
   (%sprite/asset :reader sprite/asset
                  :initarg :sprite/asset)
   (%sprite/frames :reader sprite/frames
                   :initarg :sprite/frames
                   :initform 1)
   (%sprite/spritesheet :accessor sprite/spritesheet)
   (%sprite/index :accessor sprite/index)
   (%sprite/initial-index :accessor sprite/initial-index))
  (:sorting :after render))

;;; entity hooks

(ent:define-entity-hook :attach (entity sprite)
  (setf sprite/spritesheet (asset.sprite:make-spritesheet sprite/asset)
        sprite/index (u:href (asset.sprite:sprites sprite/spritesheet)
                             sprite/name)
        sprite/initial-index sprite/index))

(ent:define-entity-hook :pre-render (entity sprite)
  (mat:set-uniforms entity :sprite.index sprite/index))

(ent:define-entity-hook :render (entity sprite)
  (gl:bind-vertex-array (asset.sprite:vao sprite/spritesheet))
  (gl:draw-arrays :triangle-strip 0 4)
  (gl:bind-vertex-array 0))
