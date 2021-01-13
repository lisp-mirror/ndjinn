(in-package #:ndjinn)

(defstruct (spritesheet
            (:constructor %make-spritesheet)
            (:predicate nil)
            (:copier nil))
  (name nil :type list)
  (spec nil :type list)
  (vao 0 :type u:ub16)
  (sprites (u:dict #'equalp) :type hash-table))

(defmethod update-shader-buffer ((object spritesheet))
  (loop :with name = (spritesheet-name object)
        :with spec = (spritesheet-spec object)
        :with count = (length spec)
        :with pos = (make-array count)
        :with size = (make-array count)
        :for sprite :in spec
        :for i :from 0
        :do (destructuring-bind (&key id x y w h &allow-other-keys) sprite
              (when (and id x y w h)
                (setf (aref pos i) (vector x y)
                      (aref size i) (vector w h)
                      (u:href (spritesheet-sprites object) id) i)))
        :finally (write-shader-buffer name :path :pos :value pos)
                 (write-shader-buffer name :path :size :value size)))

(defun find-sprite (spritesheet name)
  (or (u:href (spritesheet-sprites spritesheet) name)
      (error "Sprite ~s not found in spritesheet ~s."
             name
             (spritesheet-name spritesheet))))

(defun make-spritesheet (asset buffer-spec)
  (let ((path (resolve-path asset)))
    (with-asset-cache :spritesheet asset
        (let ((spritesheet (%make-spritesheet :name asset
                                              :spec (u:safe-read-file-form path)
                                              :vao (gl:gen-vertex-array))))
          (apply #'make-shader-buffer asset buffer-spec)
          (update-shader-buffer spritesheet)
          spritesheet))))
