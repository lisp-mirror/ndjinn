(in-package #:pyx.component)

(pyx:define-component sprite ()
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

(pyx:define-entity-hook :attach (entity sprite)
  (setf sprite/spritesheet (pyx::make-spritesheet sprite/asset)
        sprite/index (u:href (pyx::sprites sprite/spritesheet) sprite/name)
        sprite/initial-index sprite/index))

(pyx:define-entity-hook :pre-render (entity sprite)
  (pyx:set-uniforms entity :sprite.index sprite/index))

(pyx:define-entity-hook :render (entity sprite)
  (gl:bind-vertex-array (pyx::vao sprite/spritesheet))
  (gl:draw-arrays :triangle-strip 0 4)
  (gl:bind-vertex-array 0))
