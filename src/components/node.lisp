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
   (%node/disabled :accessor node/disabled
                   :initarg :node/disabled
                   :initform nil)
   (%node/pause-mode :accessor node/pause-mode
                     :initarg :node/pause-mode
                     :initform :inherit))
  (:static t))

(defun node-active-p (entity)
  (let ((scene (current-scene =context=)))
    (and (not (node/disabled entity))
         (or (not (paused scene))
             (node/root-p entity)
             (not (eq (node/pause-mode entity) :stop))))))

(defun add-child (entity &key parent)
  (with-slots (%node/parent %node/children) entity
    (when %node/parent
      (u:deletef (node/children %node/parent) entity))
    (setf %node/parent (or parent (get-root-node)))
    (push entity (node/children %node/parent))
    (dolist (child %node/children)
      (add-child child :parent entity))
    entity))

(defun collect-nodes (&optional root)
  (let ((nodes nil))
    (labels ((recurse (node)
               (when (node-active-p node)
                 (dolist (child (node/children node))
                   (recurse child)))
               (push node nodes)))
      (recurse (or root (get-root-node)))
      nodes)))

(defun map-nodes (func &optional root)
  (map nil (lambda (x) (funcall func x)) (collect-nodes root)))

(defun delete-node (entity &key reparent-children)
  (flet ((%delete ()
           (let ((parent (node/parent entity)))
             (dolist (child (node/children entity))
               (if reparent-children
                   (add-child child :parent parent)
                   (delete-node child)))
             (on-entity-delete entity)
             (%detach-components entity :remove-mixins nil)
             (deregister-prefab-entity entity)
             (when parent
               (u:deletef (node/children parent) entity))
             (values))))
    (defer-work (delete)
      (%delete))))

(defun get-root-node ()
  (node-tree (current-scene =context=)))

(defun enable-entity (entity)
  (do-nodes (node :parent entity)
    (when (has-component-p node 'render)
      (dolist (viewport (get-entity-viewports node))
        (register-render-order viewport node)))
    (setf (node/disabled node) nil)))

(defun disable-entity (entity)
  (do-nodes (node :parent entity)
    (when (has-component-p node 'render)
      (dolist (viewport (get-entity-viewports node))
        (deregister-render-order viewport node)))
    (setf (node/disabled node) t)))

;;; entity hooks

(define-entity-hook :create (entity node)
  (let ((parent (node/parent entity)))
    (unless (node/root-p entity)
      (add-child entity :parent parent))
    (when (eq (node/pause-mode entity) :inherit)
      (setf (node/pause-mode entity) (if parent
                                         (node/pause-mode parent)
                                         :stop)))))
