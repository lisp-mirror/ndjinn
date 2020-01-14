(in-package #:pyx)

(defun draw-order-comparator (x y)
  (let* ((draw-order (draw-order (spec (get-scene))))
         (x (u:href draw-order x))
         (y (u:href draw-order y)))
    (< x y)))

(defun rebuild-draw-order-tree ()
  (u:do-hash-values (viewport (viewports (get-scene)))
    (let ((tree (draw-order viewport))
          (new-tree (make-draw-order-tree)))
      (avl-tree/walk tree (lambda (x) (avl-tree/insert new-tree x)))
      (setf tree new-tree))))

(defun make-draw-order-tree ()
  (make-avl-tree
   :item-type 'render
   :key #'render/order
   :sort #'draw-order-comparator
   :hash-test #'eq))

(defun register-draw-order (viewport entity)
  (avl-tree/insert (draw-order viewport) entity))

(defun deregister-draw-order (viewport entity)
  (avl-tree/delete (draw-order viewport) entity))
