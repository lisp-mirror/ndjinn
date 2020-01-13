(in-package #:pyx)

(defclass scene ()
  ((%spec :reader spec
          :initarg :spec)
   (%loaded-p :accessor loaded-p
              :initform nil)
   (%sub-trees :reader sub-trees
               :initform (u:dict #'eq))
   (%viewports :reader viewports
               :initform (u:dict #'eq))
   (%camera :reader camera
            :initform nil)
   (%node-tree :reader node-tree)
   (%materials :reader materials
               :initform (u:dict #'eq))
   (%picking-ids :reader picking-ids
                 :initform (u:dict #'eq))
   (%released-picking-ids :accessor released-picking-ids
                          :initform nil)
   (%prefabs :reader prefabs
             :initform (u:dict #'eq))
   (%collision-system :reader collision-system
                      :initform nil)
   (%uuids :reader uuids
           :initform (u:dict #'eq))))

(u:define-printer (scene stream :identity t)
  (format stream "~s" (name (spec scene))))

(defun get-scene ()
  (current-scene *state*))

(defun get-scene-name ()
  (name (spec (get-scene))))

(defun load-scene-sub-trees (scene)
  (loop :for (binding prefab) :in (sub-trees (spec scene))
        :for entity = (load-prefab prefab)
        :do (setf (u:href (sub-trees scene) binding) entity)))

(defun load-scene-viewports (scene)
  (loop :for (view-spec sub-trees) :in (viewports (spec scene))
        :for viewport = (make-viewport view-spec)
        :do (setf (u:href (viewports scene) view-spec) viewport)))

(defun load-scene (scene-name)
  (let ((spec (meta :scenes scene-name)))
    (unless spec
      (error "Scene ~s is not defined." scene-name))
    (let ((current (get-scene))
          (scene (or (u:href (scenes *state*) scene-name)
                     (make-instance 'scene :spec spec))))
      (setf (u:href (scenes *state*) scene-name) scene
            (slot-value *state* '%current-scene) scene)
      (unless (loaded-p scene)
        (make-node-tree scene)
        (make-collision-system (collider-plan spec))
        (load-scene-viewports scene)
        (load-scene-sub-trees scene)
        (setf (loaded-p scene) t))
      (setf (slot-value *state* '%current-scene) current)
      scene)))

(defun switch-scene (scene-name)
  (let ((scene (load-scene scene-name)))
    (setf (slot-value *state* '%current-scene) scene)))

(defun recompile-scene (name)
  (let ((scene (get-scene)))
    (with-slots (%spec %prefabs %loaded-p) scene
      (when (eq name (name %spec))
        (u:do-hash-values (entities %prefabs)
          (map nil #'delete-entity entities))
        (%delete-entity (node-tree scene))
        (setf %loaded-p nil)
        (load-scene name)))))
