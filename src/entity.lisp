(in-package #:pyx)

(defun %make-entity (components args)
  (let ((entity (apply #'make-mixin components (u:hash->plist args))))
    (dolist (type components)
      (register-entity-flow-event
       :component-add-hook
       (lambda () (on-component-added entity type))))
    entity))

(defmacro make-entity ((&rest components) &body body)
  (let ((components (compute-component-order components)))
    `(%make-entity ',components (u:plist->hash (list ,@body) :test #'eq))))

(defun register-entity-flow-event (event-type hook)
  (enqueue :entity-flow (list event-type hook)))

(defun modify-entity (entity &rest args)
  (register-entity-flow-event
   :component-modify
   (lambda ()
     (apply #'reinitialize-instance entity :allow-other-keys t args))))

(defgeneric on-entity-deleted (entity)
  (:method (entity))
  (:method :after (entity)
    (deregister-prefab-entity entity)
    (remove-components entity)))

(defun delete-entity (entity &key reparent-children-p)
  (when (node/root-p entity)
    (error "Cannot remove the root entity."))
  (register-entity-flow-event
   :entity-remove
   (lambda ()
     (let ((parent (node/parent entity)))
       (dolist (child (node/children entity))
         (if reparent-children-p
             (add-child child :parent parent)
             (delete-entity child)))
       (on-entity-deleted entity)
       (a:deletef (node/children parent) entity)))))

(defgeneric on-update (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-render (entity)
  (:method-combination progn)
  (:method progn (entity)))
