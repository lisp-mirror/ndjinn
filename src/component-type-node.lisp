(in-package #:ndjinn)

(define-component node ()
  ((%node/root-p :reader node/root-p
                 :initarg :node/root-p
                 :initform nil)
   (%node/parent :reader node/parent
                 :initarg :node/parent
                 :initform nil)
   (%node/disabled :accessor node/disabled
                   :initarg :node/disabled
                   :initform nil)
   (%node/pause-mode :accessor node/pause-mode
                     :initarg :node/pause-mode
                     :initform :inherit)
   (%node/children :accessor node/children
                   :initform nil)
   (%node/prefab :accessor node/prefab
                 :initform nil)
   (%node/prefab-path :accessor node/prefab-path
                      :initform nil)
   (%node/paused :accessor node/paused
                 :initform nil))
  (:static t))

(defun reparent-node (entity parent)
  (with-slots (%node/parent) entity
    (when %node/parent
      (u:deletef (node/children %node/parent) entity))
    (setf %node/parent (or parent (get-root-node)))
    (push entity (node/children %node/parent))
    entity))

(defun add-child (entity &key parent)
  (reparent-node entity parent)
  (dolist (child (node/children entity))
    (add-child child :parent entity))
  entity)

(defun map-nodes/active (func &key root)
  (labels ((recurse (node)
             (unless (or (node/disabled node)
                         (node/paused node))
               (funcall func node)
               (dolist (child (node/children node))
                 (recurse child)))))
    (recurse (or root (get-root-node)))))

(defun map-nodes (func &key root type include-disabled include-paused)
  (labels ((recurse (node)
             (when (and (or include-disabled
                            (not (node/disabled node)))
                        (or include-paused
                            (not (node/paused node))))
               (when (or (not type)
                         (and type (has-component-p node type)))
                 (funcall func node))
               (dolist (child (node/children node))
                 (recurse child)))))
    (recurse (or root (get-root-node)))))

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
    (delay-work (delete)
      (%delete))))

(defun get-root-node ()
  (scene-node-tree (current-scene =context=)))

(defun enable-entity (entity)
  (do-nodes (node :parent entity :include-disabled t)
    (on-entity-enable node)))

(defun disable-entity (entity)
  (do-nodes/active (node :parent entity)
    (on-entity-disable node)))

;;; entity hooks

(define-entity-hook :create (entity node)
  (let ((parent (node/parent entity)))
    (unless (node/root-p entity)
      (add-child entity :parent parent))
    (when (eq (node/pause-mode entity) :inherit)
      (setf (node/pause-mode entity) (if parent
                                         (node/pause-mode parent)
                                         :stop)))))

(define-entity-hook :enable (entity node)
  (setf (node/disabled entity) nil))

(define-entity-hook :disable (entity node)
  (unless (node/root-p entity)
    (setf (node/disabled entity) t)))
