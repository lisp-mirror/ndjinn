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
  (:sorting :after render))

(defstruct (spritesheet (:constructor %make-spritesheet)
                        (:conc-name nil)
                        (:predicate nil)
                        (:copier nil))
  spritesheet-name
  spec
  (sprites (u:dict #'equalp))
  vao)

(defmethod shader:update-shader-buffer ((object spritesheet))
  (loop :with name = (spritesheet-name object)
        :with spec = (spec object)
        :with count = (length spec)
        :with pos = (make-array count)
        :with size = (make-array count)
        :for sprite :in spec
        :for i :from 0
        :do (destructuring-bind (&key id x y w h &allow-other-keys) sprite
              (when (and id x y w h)
                (setf (aref pos i) (v2:vec x y)
                      (aref size i) (v2:vec w h)
                      (u:href (sprites object) id) i)))
        :finally (shader:write-shader-buffer name :pos pos)
                 (shader:write-shader-buffer name :size size)))

(defun make-spritesheet (asset)
  (let ((path (asset:resolve-path asset)))
    (asset:with-asset-cache :spritesheet path
      (let ((spritesheet (%make-spritesheet
                          :spritesheet-name asset
                          :spec (u:safe-read-file-form path)
                          :vao (gl:create-vertex-array))))
        (shader:make-shader-buffer asset :spritesheet 'umbra.sprite:sprite)
        (shader:update-shader-buffer spritesheet)
        spritesheet))))

;;; entity hooks

(ent:define-entity-hook :attach (entity sprite)
  (setf spritesheet (make-spritesheet asset)
        index (u:href (sprites spritesheet) name)
        initial-index index))

(ent:define-entity-hook :pre-render (entity sprite)
  (mat:set-uniforms (c/render:current-material entity)
                    :sprite.index index))

(ent:define-entity-hook :render (entity sprite)
  (gl:bind-vertex-array (vao spritesheet))
  (gl:draw-arrays :triangle-strip 0 4)
  (gl:bind-vertex-array 0))
