(in-package #:pyx)

(defclass scene ()
  ((%spec :reader spec
          :initarg :spec)
   (%loaded-p :accessor loaded-p
              :initform nil)
   (%viewports :reader viewports
               :initform nil)
   (%node-tree :reader node-tree)
   (%materials :reader materials
               :initform (u:dict #'eq))
   (%prefabs :reader prefabs
             :initform (u:dict #'eq))
   (%collision-system :reader collision-system
                      :initform nil)
   (%uuids :reader uuids
           :initform (u:dict #'eq))
   (%view-tags :reader view-tags
               :initform (u:dict #'eq))))

(u:define-printer (scene stream :identity t)
  (format stream "~s" (name (spec scene))))

(defun get-scene ()
  (current-scene *state*))

(defun get-scene-name ()
  (name (spec (get-scene))))

(defun get-scene-sub-tree-viewports (scene sub-tree)
  (let (viewports)
    (dolist (viewport-spec (viewports (spec scene)))
      (destructuring-bind (viewport &optional sub-trees) viewport-spec
        (when (find sub-tree sub-trees)
          (push viewport viewports))))
    viewports))

(defun make-scene-viewports (scene)
  (loop :with manager = (make-instance 'viewport-manager)
        :for (view-spec nil) :in (viewports (spec scene))
        :for viewport = (make-viewport view-spec)
        :for i :from 0
        :do (when (zerop i)
              (setf (default manager) viewport))
            (setf (u:href (table manager) view-spec) viewport)
        :finally (setf (slot-value scene '%viewports) manager)))

(defun load-scene-sub-trees (scene)
  (loop :for (sub-tree prefab) :in (sub-trees (spec scene))
        :for viewports = (get-scene-sub-tree-viewports scene sub-tree)
        :for entity = (load-prefab prefab :viewports viewports)))

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
        (make-scene-viewports scene)
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
