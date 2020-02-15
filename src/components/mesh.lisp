(in-package #:%pyx.component)

(ent:define-component mesh ()
  ((%mesh/asset :reader mesh/asset
                :initarg :mesh/asset)
   (%mesh/name :reader mesh/name
               :initarg :mesh/name)
   (%mesh/index :reader mesh/index
                :initarg :mesh/index
                :initform 0)
   (%mesh/instances :reader mesh/instances
                    :initarg :mesh/instances
                    :initform 1)
   (%mesh/primitive :accessor mesh/primitive))
  (:sorting :after render))

;;; entity hooks

(ent:define-entity-hook :attach (entity mesh)
  (let* ((path (asset:resolve-path mesh/asset))
         (gltf (asset:with-asset-cache :mesh path
                 (mesh:load path)))
         (mesh (u:href (mesh:meshes gltf) mesh/name)))
    (unless mesh
      (error "Mesh name ~s not found in mesh file ~s." mesh/name path))
    (setf mesh/primitive (aref (mesh:primitives mesh) mesh/index))))

(ent:define-entity-hook :render (entity mesh)
  (funcall (mesh:draw-func mesh/primitive) mesh/instances))
