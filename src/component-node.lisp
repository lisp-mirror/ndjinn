(in-package #:pyx)

(define-component node ()
  ((%node/root-p :reader node/root-p
                 :initarg :node/root-p
                 :initform nil)
   (%node/parent :reader node/parent
                 :initarg :node/parent
                 :initform nil)
   (%node/children :accessor node/children
                   :initform nil)
   (%node/prefab :reader node/prefab
                 :initform nil)
   (%node/prefab-path :reader node/prefab-path
                      :initform nil)
   (%node/pickable :accessor node/pickable
                   :initarg :node/pickable
                   :initform nil))
  (:sorting :before xform)
  (:static t))

(defun add-child (entity &key parent)
  (with-slots (%node/parent %node/children) entity
    (setf %node/parent (or parent (node-tree (current-scene *state*))))
    (push entity (node/children %node/parent))
    (dolist (child %node/children)
      (add-child child :parent entity))))

(defun map-nodes (func &optional parent)
  (let ((parent (or parent (node-tree (current-scene *state*)))))
    (funcall func parent)
    (dolist (child (node/children parent))
      (map-nodes func child))))

(defun make-node-tree (scene)
  (let ((root (make-entity () :node/root-p t)))
    (setf (slot-value scene '%node-tree) root)))

;;; entity hooks

(define-hook :entity-create (entity node)
  (unless node/root-p
    (add-child entity :parent node/parent)))
