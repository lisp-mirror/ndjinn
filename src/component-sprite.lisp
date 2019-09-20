(in-package #:pyx)

(define-component sprite (:after render)
  (:spritesheet nil
   :name nil
   :index nil))

(defmethod shared-initialize :after ((instance sprite) slot-names
                                     &key sprite/file sprite/name)
  (with-slots (%sprite/spritesheet %sprite/name %sprite/index) instance
    (let ((spritesheet (make-spritesheet sprite/file)))
      (setf %sprite/spritesheet spritesheet
            %sprite/index (u:href (sprites spritesheet) sprite/name)))))

(defmethod on-render progn ((entity sprite))
  (when (has-component-p 'render entity)
    (with-slots (%sprite/spritesheet %sprite/index) entity
      (with-render entity
        ((:int :sprite.index %sprite/index)
         (:float :opacity 1.0)
         (:float :alpha-cutoff 0.1))
        (bind-texture 0 (texture-id %sprite/spritesheet))
        (gl:bind-vertex-array (vao %sprite/spritesheet))
        (gl:draw-arrays :points 0 1)
        (gl:bind-vertex-array 0)))))
