(in-package #:pyx.component)

(pyx:define-component mesh ()
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

(pyx:define-entity-hook :attach (entity mesh)
  (let* ((path (pyx:resolve-path mesh/asset))
         (gltf (pyx:with-asset-cache :mesh path
                 (pyx::load-gltf path)))
         (mesh (u:href (pyx::meshes gltf) mesh/name)))
    (unless mesh
      (error "Mesh name ~s not found in mesh file ~s." mesh/name path))
    (setf mesh/primitive (aref (pyx::primitives mesh) mesh/index))))

(pyx:define-entity-hook :render (entity mesh)
  (funcall (pyx::draw-func mesh/primitive) mesh/instances))
