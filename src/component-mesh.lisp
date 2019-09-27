(in-package #:pyx)

(define-component mesh (:after render)
  (:file nil
   :index 0
   :instances 1
   :primitives nil))

(defmethod shared-initialize :after ((instance mesh) slot-names &key)
  (with-slots (%mesh/file %mesh/index %mesh/primitives) instance
    (assert %mesh/file)
    (let ((mesh (cache-lookup :mesh (cons %mesh/file %mesh/index)
                  (load-gltf (resolve-asset-path %mesh/file) %mesh/index))))
      (setf %mesh/primitives (primitives mesh)))))

(defmethod on-render progn ((entity mesh))
  (when (has-component-p 'render entity)
    (with-slots (%mesh/primitives %mesh/instances) entity
      (dolist (primitive %mesh/primitives)
        (funcall (draw-func primitive) %mesh/instances)))))
