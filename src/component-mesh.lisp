(in-package #:%pyx.component.mesh)

(ent:define-component mesh ()
  ((%asset :reader asset
           :initarg :mesh/asset)
   (%name :reader name
          :initarg :mesh/name)
   (%index :reader index
           :initarg :mesh/index
           :initform 0)
   (%instances :reader instances
               :initarg :mesh/instances
               :initform 1)
   (%primitive :accessor primitive))
  (:sorting :after render))

;;; entity hooks

(ent:define-entity-hook :attach (entity mesh)
  (let* ((path (asset:resolve-path asset))
         (gltf (asset:with-asset-cache :mesh asset
                 (mesh:load path)))
         (mesh (u:href (mesh:meshes gltf) name)))
    (unless mesh
      (error "Mesh name ~s not found in mesh file ~s." name path))
    (setf primitive (aref (mesh:primitives mesh) index))))

(ent:define-entity-hook :render (entity mesh)
  (funcall (mesh:draw-func primitive) instances))

(ent:define-entity-hook :delete (entity mesh)
  (asset:delete-asset :mesh asset))
