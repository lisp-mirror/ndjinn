(in-package #:pyx)

(defun draw-order-comparator (x y)
  (let* ((draw-order (draw-order (pipeline (spec (current-scene *state*)))))
         (x (u:href draw-order x))
         (y (u:href draw-order y)))
    (< x y)))

(defun rebuild-draw-order-tree ()
  (let ((new-tree (make-draw-order-tree)))
    (symbol-macrolet ((tree (draw-order (current-scene *state*))))
      (tree:walk tree (lambda (x) (tree:insert new-tree x)))
      (setf tree new-tree))))

(defun make-draw-order-tree ()
  (tree:make-tree 'tree:avl-tree
                  :item-type 'render
                  :key #'render/order
                  :sort #'draw-order-comparator
                  :hash-test #'eq))

(defun register-draw-order (entity)
  (let ((order (draw-order (current-scene *state*))))
    (tree:insert order entity)))

(defun deregister-draw-order (entity)
  (let ((order (draw-order (current-scene *state*))))
    (tree:delete order entity)))
