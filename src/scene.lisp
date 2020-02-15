(in-package #:%pyx.scene)

;;; spec

(defstruct (spec (:constructor %make-spec)
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  name
  pass-order
  pass-table
  draw-order
  collider-plan
  sub-trees
  spec-viewports)

(u:define-printer (spec stream)
  (format stream "~s" (name spec)))

(defun make-scene-draw-order-table (order)
  (loop :with table = (u:dict #'eq :default 0)
        :for item :in order
        :for i :from 0
        :do (setf (u:href table item) i)
        :finally (return table)))

(defun update-spec (name sub-trees viewports passes draw-order collider-plan)
  (let* ((spec (u:href meta:=scenes= name))
         (draw-order-table (make-scene-draw-order-table draw-order))
         (pass-table (u:dict #'eq))
         (sub-trees (mapcar
                     (lambda (x)
                       (if (listp x) x (list x x)))
                     sub-trees))
         (viewports (or viewports
                        `((:default ,(mapcar #'car sub-trees))))))
    (dolist (pass passes)
      (a:if-let ((pass-spec (u:href meta:=render-passes= pass)))
        (setf (u:href pass-table pass) pass-spec)
        (error "Render pass ~s not defined." pass)))
    (setf (sub-trees spec) sub-trees
          (spec-viewports spec) viewports
          (pass-order spec) passes
          (pass-table spec) pass-table
          (draw-order spec) draw-order-table
          (collider-plan spec) collider-plan)
    (util::enqueue :recompile (list :scene name))))

(defun make-spec (name sub-trees viewports passes draw-order collider-plan)
  (let ((spec (%make-spec :name name)))
    (setf (u:href meta:=scenes= name) spec)
    (update-spec name sub-trees viewports passes draw-order collider-plan)
    spec))

(util::on-recompile :scene data ()
  (let ((scene (ctx:current-scene)))
    (with-slots (%spec %prefabs %loaded-p) scene
      (when (eq data (name %spec))
        (u:do-hash-values (entities %prefabs)
          (map nil #'c/node:delete entities))
        (c/node:delete (node-tree scene))
        (setf %loaded-p nil)
        (load data)))))

(defun get-registered-scene-names (&optional package-name)
  (let ((package-name (or package-name *package*)))
    (remove-if-not
     (lambda (x)
       (eq (symbol-package x) (find-package package-name)))
     (sort (u:hash-keys meta:=scenes=) #'string<))))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key sub-trees viewports (passes '(:default))
                         (draw-order '(:default))
                         (collider-plan :default))
      (car body)
    `(if (u:href meta:=scenes= ',name)
         (update-spec ',name ',sub-trees ',viewports ',passes ',draw-order
                      ',collider-plan)
         (make-spec ',name ',sub-trees ',viewports ',passes ',draw-order
                    ',collider-plan))))

;;; implementation

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

(defun get-scene-name ()
  (name (spec (ctx:current-scene))))

(defun switch-scene (scene-name)
  (let ((scene (load scene-name)))
    (setf (ctx:current-scene) scene)))
