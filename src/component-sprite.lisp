(in-package #:%pyx.component.sprite)

(ent:define-component sprite ()
  ((%name :reader name
          :initarg :sprite/name)
   (%texture :reader texture
             :initarg :sprite/texture)
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
  spec
  (sprites (u:dict #'equalp))
  texture-name
  vao)

(defun load-spritesheet-spec (texture-name)
  (let* ((texture-spec (tex:find-spec texture-name))
         (spec (res:resolve-path
                (make-pathname :defaults (tex:source texture-spec)
                               :type "spec"))))
    (unless (uiop:file-exists-p spec)
      (error "Spritesheet spec file ~s could not be found." spec))
    (u:safe-read-file-form spec)))

(defmethod shader:update-shader-buffer ((object spritesheet))
  (loop :with spec = (spec object)
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
        :finally (shader:write-shader-buffer (texture-name object) :pos pos)
                 (shader:write-shader-buffer (texture-name object) :size size)))

(defun make-spritesheet (texture-name)
  (res:with-resource-cache :spritesheet texture-name
    (let ((spritesheet (%make-spritesheet
                        :spec (load-spritesheet-spec texture-name)
                        :texture-name texture-name
                        :vao (gl:create-vertex-array))))
      (shader:make-shader-buffer texture-name :spritesheet 'umbra.sprite:sprite)
      (shader:update-shader-buffer spritesheet)
      spritesheet)))

;;; entity hooks

(ent:define-entity-hook :attach (entity sprite)
  (setf spritesheet (make-spritesheet texture)
        index (u:href (sprites spritesheet) name)
        initial-index index))

(ent:define-entity-hook :pre-render (entity sprite)
  (mat:set-uniforms (c/render:current-material entity)
                    :sprite.index index
                    :sprite.sampler (texture-name spritesheet)))

(ent:define-entity-hook :render (entity sprite)
  (gl:bind-vertex-array (vao spritesheet))
  (gl:draw-arrays :triangle-strip 0 4)
  (gl:bind-vertex-array 0))
