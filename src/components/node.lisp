(in-package #:%pyx.component)

(ent:define-component node ()
  ((%node/root-p :reader node/root-p
                 :initarg :node/root-p
                 :initform nil)
   (%node/parent :reader node/parent
                 :initarg :node/parent
                 :initform nil)
   (%node/children :accessor node/children
                   :initform nil)
   (%node/prefab :accessor node/prefab
                 :initform nil)
   (%node/prefab-path :accessor node/prefab-path
                      :initform nil))
  (:static t))

(defun add-child (entity &key parent)
  (with-slots (%node/parent %node/children) entity
    (setf %node/parent (or parent (scene:node-tree (ctx:current-scene))))
    (push entity (node/children %node/parent))
    (dolist (child %node/children)
      (add-child child :parent entity))))

(defun map-nodes (func &optional parent)
  (let ((parent (or parent (scene:node-tree (ctx:current-scene)))))
    (funcall func parent)
    (dolist (child (node/children parent))
      (map-nodes func child))))

(defmacro do-nodes ((entity &key parent) &body body)
  `(map-nodes (lambda (,entity) ,@body) ,parent))

(defun delete-node (entity &key reparent-children)
  (let ((parent (node/parent entity)))
    (dolist (child (node/children entity))
      (if reparent-children
          (add-child child :parent parent)
          (delete-node child)))
    (ent:on-delete entity)
    (ent:detach-components entity)
    (prefab:deregister-prefab-entity entity)
    (when parent
      (a:deletef (node/children parent) entity))
    (values)))

;;; entity hooks

(ent:define-entity-hook :create (entity node)
  (unless node/root-p
    (add-child entity :parent node/parent)))
