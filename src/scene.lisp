(in-package #:ndjinn)

;;; spec

(defun make-scene-draw-order-table (order)
  (loop :with table = (u:dict #'eq 'default 0)
        :for item :in order
        :for i :from 0
        :do (setf (u:href table item) i)
        :finally (return table)))

(defun update-scene-spec (name sub-trees viewports passes draw-order
                          collision-plan)
  (let* ((spec (u:href =meta/scenes= name))
         (draw-order-table (make-scene-draw-order-table draw-order))
         (pass-table (u:dict #'eq))
         (sub-trees (mapcar
                     (lambda (x)
                       (if (listp x) x (list x x)))
                     sub-trees))
         (viewports (or viewports `((default ,(mapcar #'car sub-trees))))))
    (dolist (pass passes)
      (u:if-let ((pass-spec (u:href =meta/render-passes= pass)))
        (setf (u:href pass-table pass) pass-spec)
        (error "Render pass ~s not defined." pass)))
    (setf (scene-spec-sub-trees spec) sub-trees
          (scene-spec-viewports spec) viewports
          (scene-spec-pass-order spec) passes
          (scene-spec-pass-table spec) pass-table
          (scene-spec-draw-order spec) draw-order-table
          (scene-spec-collision-plan spec) collision-plan)
    (enqueue :recompile (list :scene name))))

(defun make-scene-spec (name sub-trees viewports passes draw-order
                        collision-plan)
  (let ((spec (%make-scene-spec :name name)))
    (setf (u:href =meta/scenes= name) spec)
    (update-scene-spec name sub-trees viewports passes draw-order
                       collision-plan)
    spec))

(defun get-registered-scene-names (&optional package-name)
  (let ((package-name (or package-name *package*)))
    (remove-if-not
     (lambda (x)
       (eq (symbol-package x) (find-package package-name)))
     (sort (u:hash-keys =meta/scenes=) #'string<))))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key sub-trees viewports (passes '(default))
                         (draw-order '(default)) (collision-plan 'default))
      (car body)
    `(if (u:href =meta/scenes= ',name)
         (update-scene-spec ',name ',sub-trees ',viewports ',passes ',draw-order
                            ',collision-plan)
         (make-scene-spec ',name ',sub-trees ',viewports ',passes ',draw-order
                          ',collision-plan))))

;;; implementation

(defun make-scene-viewports (scene)
  (loop :with manager = (make-viewport-manager)
        :for (view-spec nil) :in (scene-spec-viewports (scene-spec scene))
        :for order = (make-render-order-tree)
        :for picker = (make-picker)
        :for viewport = (make-viewport view-spec order picker)
        :for i :from 0
        :collect view-spec :into viewport-order
        :do (when (zerop i)
              (setf (viewport-manager-default manager) viewport))
            (setf (u:href (viewport-manager-table manager) view-spec) viewport)
        :finally (setf (viewport-manager-order manager) viewport-order
                       (scene-viewports scene) manager)))

(defun get-scene-sub-tree-viewports (scene sub-tree)
  (let (viewports)
    (dolist (viewport-spec (scene-spec-viewports (scene-spec scene)))
      (destructuring-bind (viewport &optional sub-trees) viewport-spec
        (when (find sub-tree sub-trees)
          (push viewport viewports))))
    viewports))

(defun load-scene-sub-trees (scene)
  (loop :for (sub-tree prefab) :in (scene-spec-sub-trees (scene-spec scene))
        :for viewports = (get-scene-sub-tree-viewports scene sub-tree)
        :for entity = (load-prefab prefab :viewports viewports)))

(defun load-scene (scene-name)
  (let ((spec (u:href =meta/scenes= scene-name)))
    (unless spec
      (error "Scene ~s is not defined." scene-name))
    (let* ((current (current-scene =context=))
           (scenes (scenes =context=))
           (scene (or (u:href scenes scene-name)
                      (make-scene :spec spec))))
      (setf (u:href scenes scene-name) scene
            (current-scene =context=) scene
            (scene-passes scene) (copy-seq (scene-spec-pass-order
                                            (scene-spec scene))))
      (unless (scene-loaded-p scene)
        (setf (scene-node-tree scene) (make-entity ()
                                        :node/root-p t
                                        :node/pause-mode :stop
                                        :id/display "/")
              (scene-collision-system scene) (make-collision-system
                                              (scene-spec-collision-plan spec)))
        (make-scene-viewports scene)
        (load-scene-sub-trees scene)
        (setf (scene-loaded-p scene) t))
      (setf (current-scene =context=) current)
      scene)))

(defun get-scene-name ()
  (scene-spec-name (scene-spec (current-scene =context=))))

(defun switch-scene (scene-name)
  (let ((scene (load-scene scene-name)))
    (log:debug :ndjinn "Switched to scene: ~s" scene-name)
    (setf (current-scene =context=) scene)))

(on-recompile :scene data ()
  (let* ((scene (current-scene =context=))
         (name (scene-spec-name (scene-spec scene))))
    (when (eq data name)
      (u:do-hash-values (entities (scene-prefabs scene))
        (map nil #'delete-node entities))
      (delete-node (get-root-node))
      (setf (scene-loaded-p scene) nil)
      (load-scene data))
    (log:debug :ndjinn "Recompiled scene: ~s" name)))
