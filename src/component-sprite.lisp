(in-package #:pyx)

(define-component sprite (:after render)
  (:spritesheet nil
   :name nil
   :texture nil
   :index nil))

(defmethod shared-initialize :after ((instance sprite) slot-names &key)
  (with-slots (%sprite/spritesheet %sprite/name %sprite/texture %sprite/index)
      instance
    (let ((spritesheet (make-spritesheet %sprite/texture)))
      (setf %sprite/spritesheet spritesheet
            %sprite/index (u:href (sprites spritesheet) %sprite/name)))))

(defmethod on-render progn ((entity sprite))
  (with-slots (%sprite/spritesheet %sprite/index) entity
    (set-uniforms (render/material entity)
                  :sprite.index %sprite/index
                  :sprite.sampler (texture %sprite/spritesheet)
                  :opacity 1.0
                  :alpha-cutoff 0.1)
    (gl:bind-vertex-array (vao %sprite/spritesheet))
    (gl:draw-arrays :points 0 1)
    (gl:bind-vertex-array 0)))
