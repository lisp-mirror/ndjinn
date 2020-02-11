(in-package #:%pyx.scene)

(defclass scene ()
  ((%spec :reader spec
          :initarg :spec)
   (%loaded-p :accessor loaded-p
              :initform nil)
   (%viewports :reader viewports
               :initform nil)
   (%node-tree :accessor node-tree)
   (%materials :reader materials
               :initform (u:dict #'eq))
   (%passes :accessor passes
            :initform nil)
   (%prefabs :reader prefabs
             :initform (u:dict #'eq))
   (%collision-system :accessor collision-system
                      :initform nil)
   (%uuids :reader uuids
           :initform (u:dict #'eq))))

(u:define-printer (scene stream :identity t)
  (format stream "~s" (name (spec scene))))

(defun make-viewports (scene)
  (loop :with manager = (vp:make-manager)
        :for (view-spec nil) :in (spec-viewports (spec scene))
        :for order = (render:make-order-tree)
        :for ray = (cd:make-picking-ray)
        :for viewport = (vp:make-viewport view-spec order ray)
        :for i :from 0
        :do (when (zerop i)
              (setf (vp:default manager) viewport))
            (setf (u:href (vp:table manager) view-spec) viewport)
        :finally (setf (slot-value scene '%viewports) manager)))

(defun get-sub-tree-viewports (scene sub-tree)
  (let (viewports)
    (dolist (viewport-spec (spec-viewports (spec scene)))
      (destructuring-bind (viewport &optional sub-trees) viewport-spec
        (when (find sub-tree sub-trees)
          (push viewport viewports))))
    viewports))

(defun load-sub-trees (scene)
  (loop :for (sub-tree prefab) :in (sub-trees (spec scene))
        :for viewports = (get-sub-tree-viewports scene sub-tree)
        :for entity = (prefab:load-prefab prefab :viewports viewports)))

(defun load (scene-name)
  (let ((spec (u:href meta:=scenes= scene-name)))
    (unless spec
      (error "Scene ~s is not defined." scene-name))
    (let ((current (ctx:current-scene))
          (scene (or (u:href (ctx:scenes) scene-name)
                     (make-instance 'scene :spec spec))))
      (setf (u:href (ctx:scenes) scene-name) scene
            (ctx:current-scene) scene
            (passes scene) (copy-seq (pass-order (spec scene))))
      (unless (loaded-p scene)
        (setf (node-tree scene) (ent:make-entity () :node/root-p t)
              (collision-system scene) (cd:make-collision-system
                                        (collider-plan spec)))
        (make-viewports scene)
        (load-sub-trees scene)
        (setf (loaded-p scene) t))
      (setf (ctx:current-scene) current)
      scene)))

;;; Public API

(defun get-scene-name ()
  (name (spec (ctx:current-scene))))

(defun switch-scene (scene-name)
  (let ((scene (load scene-name)))
    (setf (ctx:current-scene) scene)))
