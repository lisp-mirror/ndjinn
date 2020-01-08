(in-package #:pyx)

(define-component mesh ()
  ((%mesh/file :reader mesh/file
               :initarg :mesh/file)
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

(define-hook :attach (entity mesh)
  (let* ((gltf (resource-lookup 'mesh mesh/file
                 (load-gltf (resolve-asset-path mesh/file))))
         (mesh (u:href (meshes gltf) mesh/name)))
    (unless mesh
      (error "Mesh name ~s not found in glTF file ~s." mesh/name mesh/file))
    (setf mesh/primitive (aref (primitives mesh) mesh/index))))

(define-hook :render (entity mesh)
  (funcall (draw-func mesh/primitive) mesh/instances))
