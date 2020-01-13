(in-package #:pyx)

(defun draw-order-comparator (x y)
  (let* ((draw-order (draw-order (spec (get-scene))))
         (x (u:href draw-order x))
         (y (u:href draw-order y)))
    (< x y)))

(defun rebuild-draw-order-tree ()
  (let ((new-tree (make-draw-order-tree)))
    (symbol-macrolet ((tree (draw-order (get-scene))))
      (avl-tree/walk tree (lambda (x) (avl-tree/insert new-tree x)))
      (setf tree new-tree))))

(defun make-draw-order-tree ()
  (make-avl-tree
   :item-type 'render
   :key #'render/order
   :sort #'draw-order-comparator
   :hash-test #'eq))

(defun register-draw-order (entity)
  (let ((order (draw-order (current-scene *state*))))
    (avl-tree/insert order entity)))

(defun deregister-draw-order (entity)
  (let ((order (draw-order (current-scene *state*))))
    (avl-tree/delete order entity)))
