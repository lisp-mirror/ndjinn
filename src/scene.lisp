(in-package #:pyx)

(defclass scene ()
  ((%spec :reader spec
          :initarg :spec)
   (%loaded-p :accessor loaded-p
              :initform nil)
   (%camera :reader camera
            :initform nil)
   (%node-tree :reader node-tree)
   (%materials :reader materials
               :initform (u:dict #'eq))
   (%draw-order :reader draw-order
                :initform (u:dict #'eq))
   (%picking-ids :reader picking-ids
                 :initform (u:dict #'eq))
   (%released-picking-ids :accessor released-picking-ids
                          :initform nil)
   (%prefabs :reader prefabs
             :initform (u:dict #'eq))
   (%uuids :reader uuids
           :initform (u:dict #'eq))))

(u:define-printer (scene stream)
  (format stream "~s" (name (spec scene))))

(defun get-current-scene-name ()
  (name (spec (current-scene *state*))))

(defun load-scene (scene-name)
  (let ((spec (meta :scenes scene-name)))
    (unless spec
      (error "Scene ~s is not defined." scene-name))
    (let ((current (current-scene *state*))
          (scene (or (u:href (scenes *state*) scene-name)
                     (make-instance 'scene :spec spec))))
      (setf (u:href (scenes *state*) scene-name) scene
            (slot-value *state* '%current-scene) scene)
      (unless (loaded-p scene)
        (make-node-tree scene)
        (dolist (prefab (prefabs spec))
          (load-prefab prefab))
        (setf (loaded-p scene) t))
      (setf (slot-value *state* '%current-scene) current)
      scene)))

(defun switch-scene (scene-name)
  (let ((scene (load-scene scene-name)))
    (setf (slot-value *state* '%current-scene) scene)))

(defun recompile-scene (name)
  (with-slots (%spec %prefabs %loaded-p) (current-scene *state*)
    (when (eq name (name %spec))
      (u:do-hash-values (entities %prefabs)
        (map nil #'delete-entity entities))
      (setf %loaded-p nil)
      (load-scene name))))
