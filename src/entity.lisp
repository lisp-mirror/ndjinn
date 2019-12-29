(in-package #:pyx)

(defun get-entity-slots (types)
  (mapcan
   (lambda (x)
     (mapcar
      #'c2mop:slot-definition-name
      (c2mop:class-slots (find-class x))))
   types))

(defun make-entity-class (components)
  (make-mixin-class (make-mixin-class-list components)))

(defun register-entity (entity types)
  (on-entity-create entity)
  (dolist (type types)
    (on-component-attach entity type)))

(defun %make-entity (types &optional args)
  (let* ((class (make-entity-class types))
         (entity (apply #'make-instance class
                        (when args (u:hash->plist args)))))
    (register-entity entity types)
    entity))

(defmacro make-entity ((&rest components) &body body)
  (let ((components (compute-component-order components)))
    `(%make-entity ',components (u:plist->hash (list ,@body) :test #'eq))))

(defun %delete-entity (entity &key reparent-children-p)
  (let ((parent (node/parent entity)))
    (dolist (child (node/children entity))
      (if reparent-children-p
          (add-child child :parent parent)
          (%delete-entity child)))
    (on-entity-delete entity)
    (detach-components entity)
    (deregister-prefab-entity entity)
    (when parent
      (a:deletef (node/children parent) entity))))

(defun delete-entity (entity &key reparent-children-p)
  (when (node/root-p entity)
    (error "Cannot remove the root entity."))
  (%delete-entity entity :reparent-children-p reparent-children-p))
