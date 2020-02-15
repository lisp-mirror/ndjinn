(in-package #:%pyx.component.node)

(ent:define-component node ()
  ((%root-p :reader root-p
            :initarg :node/root-p
            :initform nil)
   (%parent :reader parent
            :initarg :node/parent
            :initform nil)
   (%children :accessor children
              :initform nil)
   (%prefab :accessor prefab
            :initform nil)
   (%prefab-path :accessor prefab-path
                 :initform nil))
  (:static t))

(defun add-child (entity &key parent)
  (with-slots (%parent %children) entity
    (setf %parent (or parent (scene:node-tree (ctx:current-scene))))
    (push entity (children %parent))
    (dolist (child %children)
      (add-child child :parent entity))))

(defun map-nodes (func &optional parent)
  (let ((parent (or parent (scene:node-tree (ctx:current-scene)))))
    (funcall func parent)
    (dolist (child (children parent))
      (map-nodes func child))))

(defmacro do-nodes ((entity &key parent) &body body)
  `(map-nodes (lambda (,entity) ,@body) ,parent))

(defun delete (entity &key reparent-children)
  (let ((parent (parent entity)))
    (dolist (child (children entity))
      (if reparent-children
          (add-child child :parent parent)
          (delete child)))
    (ent:on-delete entity)
    (ent:detach-components entity)
    (prefab:deregister-prefab-entity entity)
    (when parent
      (a:deletef (children parent) entity))
    (values)))

;;; entity hooks

(ent:define-entity-hook :create (entity node)
  (unless root-p
    (add-child entity :parent parent)))
