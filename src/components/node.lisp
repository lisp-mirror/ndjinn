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
                      :initform nil)
   (%node/pause-mode :accessor node/pause-mode
                     :initarg :node/pause-mode
                     :initform :inherit))
  (:static t))

(defun add-child (entity &key parent)
  (with-slots (%node/parent %node/children) entity
    (setf %node/parent (or parent (node-tree (current-scene =context=))))
    (push entity (node/children %node/parent))
    (dolist (child %node/children)
      (add-child child :parent entity))))

(defun map-nodes (func &optional root)
  (let* ((scene (current-scene =context=))
         (root (or root (node-tree scene))))
    (funcall func root)
    (dolist (child (node/children root))
      (unless (and (paused scene)
                   (eq (node/pause-mode child) :stop))
        (map-nodes func child)))))

(defun delete-node (entity &key reparent-children)
  (flet ((%delete ()
           (let ((parent (node/parent entity)))
             (dolist (child (node/children entity))
               (if reparent-children
                   (add-child child :parent parent)
                   (delete-node child)))
             (on-entity-delete entity)
             (detach-components entity :remove-mixins nil)
             (deregister-prefab-entity entity)
             (when parent
               (u:deletef (node/children parent) entity))
             (values))))
    (queue-flow-work 'delete (lambda () (%delete)))))

;;; entity hooks

(define-entity-hook :create (entity node)
  (let ((parent (node/parent entity)))
    (unless (node/root-p entity)
      (add-child entity :parent parent))
    (when (eq (node/pause-mode entity) :inherit)
      (setf (node/pause-mode entity) (node/pause-mode parent)))))
