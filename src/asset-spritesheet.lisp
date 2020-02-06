(in-package #:%pyx.asset.spritesheet)

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
    (asset:with-asset-cache :spritesheet asset
      (let ((spritesheet (%make-spritesheet
                          :spritesheet-name asset
                          :spec (u:safe-read-file-form path)
                          :vao (gl:create-vertex-array))))
        (shader:make-shader-buffer asset :spritesheet 'umbra.sprite:sprite)
        (shader:update-shader-buffer spritesheet)
        spritesheet))))
