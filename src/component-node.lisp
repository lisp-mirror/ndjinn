(in-package #:pyx)

(define-component node (:before xform)
  (:root-p nil
   :parent nil
   :children nil))

(defun add-child (entity &key parent)
  (with-slots (%node/parent %node/children) entity
    (setf %node/parent (or parent (scene-graph *state*)))
    (push entity (node/children %node/parent))
    (dolist (child %node/children)
      (add-child child :parent entity))))

(defun map-nodes (func &optional parent)
  (let ((parent (or parent (scene-graph *state*))))
    (funcall func parent)
    (dolist (child (node/children parent))
      (map-nodes func child))))

(defun make-scene-graph ()
  (let ((root (make-entity () :node/root-p t)))
    (setf (slot-value *state* '%scene-graph) root)))

(defmethod on-component-added ((component (eql 'node)) entity)
  (unless (node/root-p entity)
    (add-child entity :parent (node/parent entity))))
