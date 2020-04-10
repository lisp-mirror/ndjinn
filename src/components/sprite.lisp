(in-package #:pyx.component)

(pyx:define-component sprite ()
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
   (%sprite/spritesheet :accessor sprite/spritesheet)
   (%sprite/index :accessor sprite/index)
   (%sprite/initial-index :accessor sprite/initial-index)
   (%sprite/elapsed :accessor sprite/elapsed
                    :initform 0)
   (%sprite/pause :accessor sprite/pause
                  :initform nil))
  (:sorting :after render))

;;; entity hooks

(pyx:define-entity-hook :attach (entity sprite)
  (setf sprite/spritesheet (pyx::make-spritesheet sprite/asset)
        sprite/index (u:href (pyx::sprites sprite/spritesheet) sprite/name)
        sprite/initial-index sprite/index))

(pyx:define-entity-hook :update (entity sprite)
  (unless sprite/pause
    (incf sprite/elapsed (pyx:get-frame-time))
    (if (>= sprite/elapsed sprite/duration)
        (setf sprite/elapsed 0
              sprite/pause (unless sprite/repeat t))
        (let* ((step (/ sprite/elapsed sprite/duration))
               (min sprite/initial-index)
               (max (1- (+ min sprite/frames)))
               (index (floor (a:clamp (a:lerp step min (1+ max)) min max))))
          (setf sprite/index index)))))

(pyx:define-entity-hook :pre-render (entity sprite)
  (pyx:set-uniforms entity :sprite.index sprite/index))

(pyx:define-entity-hook :render (entity sprite)
  (gl:bind-vertex-array (pyx::vao sprite/spritesheet))
  (gl:draw-arrays :triangle-strip 0 4)
  (gl:bind-vertex-array 0))
