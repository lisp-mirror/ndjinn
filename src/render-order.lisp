(in-package #:%pyx.render)

(defun order-comparator (x y)
  (let ((order (scene:draw-order (scene:spec (ctx:current-scene)))))
    (< (u:href order x)
       (u:href order y))))

(defun make-order-tree ()
  (util::make-avl-tree :item-type 'c/render:render
                       :key #'c/render:order
                       :sort #'order-comparator
                       :hash-test #'eq))

(defun register-order (viewport entity)
  (util::avl-insert (vp:draw-order viewport) entity))

(defun deregister-order (viewport entity)
  (util::avl-delete (vp:draw-order viewport) entity))
