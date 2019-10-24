(in-package #:pyx)

(defclass spritesheet ()
  ((%spec :reader spec
          :initarg :spec)
   (%sprites :reader sprites
             :initform (u:dict #'equalp))
   (%texture :reader texture
             :initarg :texture)
   (%vao :reader vao
         :initarg :vao)))

(defun load-spritesheet-spec (texture-name)
  (u:if-found (texture-spec (find-texture-spec texture-name))
              (u:safe-read-file-form
               (resolve-asset-path
                (make-pathname :defaults (source texture-spec) :type "spec")))
              (error "Texture ~s has no spritesheet spec file."
                     (name texture-name))))

(defmethod update-shader-buffer ((object spritesheet) buffer &key)
  (with-slots (%spec %sprites) object
    (loop :with count = (length %spec)
          :with pos = (make-array count)
          :with size = (make-array count)
          :for sprite :in %spec
          :for i :from 0
          :do (destructuring-bind (&key id x y w h &allow-other-keys) sprite
                (when (and id x y w h)
                  (setf (aref pos i) (v2:vec x y)
                        (aref size i) (v2:vec w h)
                        (u:href %sprites id) i)))
          :finally (shadow:write-buffer-path buffer :pos pos)
                   (shadow:write-buffer-path buffer :size size))))

(defun make-spritesheet (texture-name)
  (resource-lookup :spritesheet texture-name
    (let ((spritesheet (make-instance 'spritesheet
                                      :spec (load-spritesheet-spec texture-name)
                                      :texture texture-name
                                      :vao (gl:gen-vertex-array))))
      (make-shader-buffer :spritesheet 'umbra.sprite:sprite)
      (update-shader-buffer spritesheet :spritesheet)
      spritesheet)))
