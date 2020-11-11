(in-package #:ndjinn)

(defclass spritesheet ()
  ((%name :reader name
          :initarg :name)
   (%spec :reader spec
          :initarg :spec)
   (%vao :reader vao
         :initarg :vao)
   (%sprites :reader sprites
             :initform (u:dict #'equalp))))

(defmethod update-shader-buffer ((object spritesheet))
  (loop :with name = (name object)
        :with spec = (spec object)
        :with count = (length spec)
        :with pos = (make-array count)
        :with size = (make-array count)
        :for sprite :in spec
        :for i :from 0
        :do (destructuring-bind (&key id x y w h &allow-other-keys) sprite
              (when (and id x y w h)
                (setf (aref pos i) (vector x y)
                      (aref size i) (vector w h)
                      (u:href (sprites object) id) i)))
        :finally (write-shader-buffer name :pos pos)
                 (write-shader-buffer name :size size)))

(defun find-sprite (spritesheet name)
  (or (u:href (sprites spritesheet) name)
      (error "Sprite ~s not found in spritesheet ~s." name (name spritesheet))))

(defun make-spritesheet (asset buffer-spec)
  (let ((path (resolve-path asset)))
    (with-asset-cache :spritesheet asset
      (let ((spritesheet (make-instance 'spritesheet
                                        :name asset
                                        :spec (u:safe-read-file-form path)
                                        :vao (gl:gen-vertex-array))))
        (apply #'make-shader-buffer asset buffer-spec)
        (update-shader-buffer spritesheet)
        spritesheet))))
