(in-package #:net.mfiano.lisp.pyx)

;;; spec

(defclass scene-spec ()
  ((%name :reader name
          :initarg :name)
   (%pass-order :accessor pass-order)
   (%pass-table :accessor pass-table)
   (%draw-order :accessor draw-order)
   (%collider-plan :accessor collider-plan)
   (%sub-trees :accessor sub-trees)
   (%viewports :accessor viewports)))

(u:define-printer (scene-spec stream)
  (format stream "~s" (name scene-spec)))

(defun make-scene-draw-order-table (order)
  (loop :with table = (u:dict #'eq 'default 0)
        :for item :in order
        :for i :from 0
        :do (setf (u:href table item) i)
        :finally (return table)))

(defun update-scene-spec (name sub-trees viewports passes draw-order
                          collider-plan)
  (let* ((spec (u:href =scenes= name))
         (draw-order-table (make-scene-draw-order-table draw-order))
         (pass-table (u:dict #'eq))
         (sub-trees (mapcar
                     (lambda (x)
                       (if (listp x) x (list x x)))
                     sub-trees))
         (viewports (or viewports `((default ,(mapcar #'car sub-trees))))))
    (dolist (pass passes)
      (u:if-let ((pass-spec (u:href =render-passes= pass)))
        (setf (u:href pass-table pass) pass-spec)
        (error "Render pass ~s not defined." pass)))
    (setf (sub-trees spec) sub-trees
          (viewports spec) viewports
          (pass-order spec) passes
          (pass-table spec) pass-table
          (draw-order spec) draw-order-table
          (collider-plan spec) collider-plan)
    (enqueue :recompile (list :scene name))))

(defun make-scene-spec (name sub-trees viewports passes draw-order
                        collider-plan)
  (let ((spec (make-instance 'scene-spec :name name)))
    (setf (u:href =scenes= name) spec)
    (update-scene-spec name sub-trees viewports passes draw-order collider-plan)
    spec))

(on-recompile :scene data ()
  (let ((scene (current-scene)))
    (with-slots (%spec %prefabs %loaded-p) scene
      (when (eq data (name %spec))
        (u:do-hash-values (entities %prefabs)
          (map nil #'delete-node entities))
        (delete-node (node-tree scene))
        (setf %loaded-p nil)
        (load-scene data)))))

(defun get-registered-scene-names (&optional package-name)
  (let ((package-name (or package-name *package*)))
    (remove-if-not
     (lambda (x)
       (eq (symbol-package x) (find-package package-name)))
     (sort (u:hash-keys =scenes=) #'string<))))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key sub-trees viewports (passes '(default))
                         (draw-order '(default)) (collider-plan 'default))
      (car body)
    `(if (u:href =scenes= ',name)
         (update-scene-spec ',name ',sub-trees ',viewports ',passes ',draw-order
                            ',collider-plan)
         (make-scene-spec ',name ',sub-trees ',viewports ',passes ',draw-order
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
   (%picked-entity :accessor picked-entity
                   :initform nil)
   (%uuids :reader uuids
           :initform (u:dict #'eq))))

(u:define-printer (scene stream :identity t)
  (format stream "~s" (name (spec scene))))

(defun make-scene-viewports (scene)
  (loop :with manager = (make-instance 'viewport-manager)
        :for (view-spec nil) :in (viewports (spec scene))
        :for order = (make-render-order-tree)
        :for ray = (make-picking-ray)
        :for viewport = (make-viewport view-spec order ray)
        :for i :from 0
        :do (when (zerop i)
              (setf (default manager) viewport))
            (setf (u:href (table manager) view-spec) viewport)
        :finally (setf (slot-value scene '%viewports) manager)))

(defun get-scene-sub-tree-viewports (scene sub-tree)
  (let (viewports)
    (dolist (viewport-spec (viewports (spec scene)))
      (destructuring-bind (viewport &optional sub-trees) viewport-spec
        (when (find sub-tree sub-trees)
          (push viewport viewports))))
    viewports))

(defun load-scene-sub-trees (scene)
  (loop :for (sub-tree prefab) :in (sub-trees (spec scene))
        :for viewports = (get-scene-sub-tree-viewports scene sub-tree)
        :for entity = (load-prefab prefab :viewports viewports)))

(defun load-scene (scene-name)
  (let ((spec (u:href =scenes= scene-name)))
    (unless spec
      (error "Scene ~s is not defined." scene-name))
    (let ((current (current-scene))
          (scene (or (u:href (scenes) scene-name)
                     (make-instance 'scene :spec spec))))
      (setf (u:href (scenes) scene-name) scene
            (current-scene) scene
            (passes scene) (copy-seq (pass-order (spec scene))))
      (unless (loaded-p scene)
        (setf (node-tree scene) (make-entity () :node/root-p t)
              (collision-system scene) (make-collision-system
                                        (collider-plan spec)))
        (make-scene-viewports scene)
        (load-scene-sub-trees scene)
        (setf (loaded-p scene) t))
      (setf (current-scene) current)
      scene)))

(defun get-scene-name ()
  (name (spec (current-scene))))

(defun switch-scene (scene-name)
  (let ((scene (load-scene scene-name)))
    (setf (current-scene) scene)))
