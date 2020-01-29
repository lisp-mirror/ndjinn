(in-package #:%pyx.component.mesh)

(ent:define-component mesh ()
  ((%file :reader file
          :initarg :mesh/file)
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
  (let* ((gltf (res:with-resource-cache :mesh file
                 (res.mesh:load (res:resolve-path file))))
         (mesh (u:href (res.mesh:meshes gltf) name)))
    (unless mesh
      (error "Mesh name ~s not found in mesh file ~s." name file))
    (setf primitive (aref (res.mesh:primitives mesh) index))))

(ent:define-entity-hook :render (entity mesh)
  (funcall (res.mesh:draw-func primitive) instances))
