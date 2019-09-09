(in-package #:pyx)

(defclass node ()
  ((%parent :accessor parent
            :initform nil)
   (%children :accessor children
              :initform nil)))

(defun add-child (node &key parent)
  (with-slots (%game-state %parent %children) node
    (setf %parent (or parent (scene-graph %game-state)))
    (push node (children %parent))
    (dolist (child %children)
      (add-child child :parent node))))

(defun map-nodes (game-state func &optional parent)
  (let ((parent (or parent (scene-graph game-state))))
    (funcall func parent)
    (dolist (child (children parent))
      (map-nodes game-state func child))))

(defun make-scene-graph (game-state)
  (setf (slot-value game-state '%scene-graph) (make-game-object game-state)))
