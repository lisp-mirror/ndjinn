(in-package #:ndjinn)

(define-component sprite ()
  ((%sprite/name :reader sprite/name
                 :initarg :sprite/name)
   (%sprite/asset :reader sprite/asset
                  :initarg :sprite/asset)
   (%sprite/frames :reader sprite/frames
                   :initarg :sprite/frames
                   :initform 1)
   (%sprite/duration :reader sprite/duration
                     :initarg :sprite/duration
                     :initform 1)
   (%sprite/repeat :reader sprite/repeat
                   :initarg :sprite/repeat
                   :initform t)
   (%sprite/instances :reader sprite/instances
                      :initarg :sprite/instances
                      :initform 1)
   (%sprite/buffer-spec :reader sprite/buffer-spec
                        :initarg :sprite/buffer-spec
                        :initform '(:spritesheet umbra.sprite:sprite))
   (%sprite/spritesheet :accessor sprite/spritesheet)
   (%sprite/index :accessor sprite/index)
   (%sprite/initial-index :accessor sprite/initial-index)
   (%sprite/elapsed :accessor sprite/elapsed
                    :initform 0)
   (%sprite/pause :accessor sprite/pause
                  :initform nil))
  (:type-order :after render))

;;; entity hooks

(define-entity-hook :attach (entity sprite)
  (let ((spritesheet (make-spritesheet (sprite/asset entity)
                                       (sprite/buffer-spec entity))))
    (setf (sprite/spritesheet entity) spritesheet
          (sprite/index entity) (find-sprite spritesheet (sprite/name entity))
          (sprite/initial-index entity) (sprite/index entity))))

(define-entity-hook :update (entity sprite)
  (unless (sprite/pause entity)
    (let ((duration (sprite/duration entity)))
      (incf (sprite/elapsed entity) (get-frame-time))
      (if (>= (sprite/elapsed entity) duration)
          (setf (sprite/elapsed entity) 0
                (sprite/pause entity) (unless (sprite/repeat entity) t))
          (let* ((step (/ (sprite/elapsed entity) duration))
                 (min (sprite/initial-index entity))
                 (max (1- (+ min (sprite/frames entity))))
                 (index (floor (u:clamp (u:lerp step min (1+ max)) min max))))
            (setf (sprite/index entity) index))))))

(define-entity-hook :pre-render (entity sprite)
  (set-uniforms entity :sprite.index (sprite/index entity)))

(define-entity-hook :render (entity sprite)
  (let ((asset (sprite/asset entity)))
    (with-shader-buffers (asset)
      (gl:bind-vertex-array (vao (sprite/spritesheet entity)))
      (gl:draw-arrays-instanced :triangle-strip 0 4 (sprite/instances entity))
      (gl:bind-vertex-array 0))))
