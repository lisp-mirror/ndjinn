(in-package #:net.mfiano.lisp.pyx)

(defun render-order-comparator (x y)
  (let ((order (draw-order (spec (current-scene)))))
    (< (u:href order x)
       (u:href order y))))

(defun make-render-order-tree ()
  (avl:make-tree :item-type 'render
                 :key #'render/order
                 :sort #'render-order-comparator
                 :hash-test #'eq))

(defun register-render-order (viewport entity)
  (avl:insert (draw-order viewport) entity))

(defun deregister-render-order (viewport entity)
  (avl:delete (draw-order viewport) entity))
