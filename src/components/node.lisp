(in-package #:net.mfiano.lisp.pyx)

(define-component node ()
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
    (setf %node/parent (or parent (node-tree (current-scene =context=))))
    (push entity (node/children %node/parent))
    (dolist (child %node/children)
      (add-child child :parent entity))))

(defun map-nodes (func &optional parent)
  (let ((parent (or parent (node-tree (current-scene =context=)))))
    (funcall func parent)
    (dolist (child (node/children parent))
      (map-nodes func child))))

(defun delete-node (entity &key reparent-children)
  (let ((parent (node/parent entity)))
    (dolist (child (node/children entity))
      (if reparent-children
          (add-child child :parent parent)
          (delete-node child)))
    (on-delete entity)
    (detach-components entity)
    (deregister-prefab-entity entity)
    (when parent
      (u:deletef (node/children parent) entity))
    (values)))

;;; entity hooks

(define-entity-hook :create (entity node)
  (unless node/root-p
    (add-child entity :parent node/parent)))
