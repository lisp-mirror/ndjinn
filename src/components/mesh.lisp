(in-package #:net.mfiano.lisp.pyx)

(define-component mesh ()
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
  (:type-order :after render))

;;; entity hooks

(define-entity-hook :attach (entity mesh)
  (let* ((path (resolve-path mesh/asset))
         (gltf (with-asset-cache :mesh path
                 (prog1 (load-gltf path)
                   (log:debug :pyx.comp "Cached mesh asset: ~a" path))))
         (mesh (u:href (meshes gltf) mesh/name)))
    (unless mesh
      (error "Mesh name ~s not found in mesh file ~s." mesh/name path))
    (setf mesh/primitive (aref (primitives mesh) mesh/index))))

(define-entity-hook :render (entity mesh)
  (funcall (draw-func mesh/primitive) mesh/instances))
