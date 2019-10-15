(in-package #:pyx)

(define-component mesh (:after render)
  (:file nil
   :primitive nil
   :name nil
   :index 0
   :instances 1))

(defmethod shared-initialize :after ((instance mesh) slot-names &key)
  (with-slots (%mesh/file %mesh/name %mesh/index %mesh/primitive) instance
    (assert %mesh/file)
    (assert %mesh/name)
    (let* ((gltf (cache-lookup :mesh %mesh/file
                   (load-gltf (resolve-asset-path %mesh/file))))
           (mesh (u:href (meshes gltf) %mesh/name)))
      (unless mesh
        (error "Mesh name ~s not found in glTF file ~s." %mesh/name %mesh/file))
      (setf %mesh/primitive (aref (primitives mesh) %mesh/index)))))

(defmethod on-render progn ((entity mesh))
  (with-slots (%mesh/primitive %mesh/instances) entity
    (funcall (draw-func %mesh/primitive) %mesh/instances)))
