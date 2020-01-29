(in-package #:%pyx.render)

(defun order-comparator (x y)
  (let ((order (scene:draw-order (scene:spec (ctx:current-scene)))))
    (< (u:href order x)
       (u:href order y))))

(defun make-order-tree ()
  (avl:make-tree :item-type 'c/render:render
                 :key #'c/render:order
                 :sort #'order-comparator
                 :hash-test #'eq))

(defun register-order (viewport entity)
  (avl:insert (vp:draw-order viewport) entity))

(defun deregister-order (viewport entity)
  (avl:delete (vp:draw-order viewport) entity))
