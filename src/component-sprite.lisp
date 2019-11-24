(in-package #:pyx)

(define-component sprite ()
  ((%sprite/name :reader sprite/name
                 :initarg :sprite/name)
   (%sprite/texture :reader sprite/texture
                    :initarg :sprite/texture
                    :initform 'default)
   (%sprite/frames :reader sprite/frames
                   :initarg :sprite/frames
                   :initform 1)
   (%sprite/spritesheet :reader sprite/spritesheet)
   (%sprite/index :reader sprite/index)
   (%sprite/initial-index :reader sprite/initial-index))
  (:sorting :after render))

(defmethod shared-initialize :after ((instance sprite) slot-names &key)
  (with-slots (%sprite/spritesheet %sprite/name %sprite/texture %sprite/index
               %sprite/initial-index)
      instance
    (let ((spritesheet (make-spritesheet %sprite/texture)))
      (setf %sprite/spritesheet spritesheet
            %sprite/index (u:href (sprites spritesheet) %sprite/name)
            %sprite/initial-index %sprite/index))))

(defmethod on-render progn ((entity sprite))
  (with-slots (%sprite/spritesheet %sprite/index) entity
    (set-uniforms (render/material entity)
                  :sprite.index %sprite/index
                  :sprite.sampler (texture %sprite/spritesheet))
    (gl:bind-vertex-array (vao %sprite/spritesheet))
    (gl:draw-arrays :points 0 1)
    (gl:bind-vertex-array 0)))
