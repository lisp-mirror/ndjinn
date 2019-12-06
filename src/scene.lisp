(in-package #:pyx)

(defclass scene ()
  ((%spec :reader spec
          :initarg :spec)
   (%camera :reader camera
            :initform nil)
   (%node-tree :reader node-tree)
   (%materials :reader materials
               :initform (u:dict #'eq))
   (%groups :reader groups
            :initform (u:dict #'eq))
   (%draw-order-groups :accessor draw-order-groups
                       :initform '(default))
   (%draw-order-entities :accessor draw-order-entities
                         :initform nil)
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

(defun load-scene (scene-name)
  (let ((spec (meta :scenes scene-name)))
    (unless spec
      (error "Scene ~s is not defined." scene-name))
    (let ((scene (make-instance 'scene :spec spec)))
      (setf (u:href (scenes *state*) scene-name) scene
            (slot-value *state* '%current-scene) scene)
      (make-node-tree scene)
      (dolist (prefab (prefabs spec))
        (load-prefab prefab)))))

(defun recompile-scene (name)
  (with-slots (%spec %prefabs) (current-scene *state*)
    (when (eq name (name %spec))
      (u:do-hash-values (entities %prefabs)
        (map nil #'delete-entity entities))
      (load-scene name))))
