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
  (let* ((name (mesh/name entity))
         (path (resolve-path (mesh/asset entity)))
         (gltf (with-asset-cache :mesh path
                 (prog1 (load-gltf path)
                   (log:debug :pyx.comp "Cached mesh asset: ~a" path))))
         (mesh (u:href (meshes gltf) name)))
    (unless mesh
      (error "Mesh name ~s not found in mesh file ~s." name path))
    (setf (mesh/primitive entity) (aref (primitives mesh)
                                        (mesh/index entity)))))

(define-entity-hook :render (entity mesh)
  (funcall (draw-func (mesh/primitive entity)) (mesh/instances entity)))
