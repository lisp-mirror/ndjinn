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

(defun register-entity (entity components)
  (dolist (type components)
    (register-entity-flow-event
     :component-add-hook
     (lambda ()
       (on-component-added entity type)))))

(defun %make-entity (components &optional args)
  (let* ((class (make-entity-class components))
         (entity (apply #'make-instance class
                        (when args (u:hash->plist args)))))
    (register-entity entity components)
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
  (:method-combination progn)
  (:method :around (entity)
    (call-next-method)
    (remove-components entity)
    (deregister-prefab-entity entity)))

(defun delete-entity (entity &key reparent-children-p)
  (when (node/root-p entity)
    (error "Cannot remove the root entity."))
  (register-entity-flow-event
   :entity-remove
   (lambda ()
     (labels ((%delete (entity)
                (let ((parent (node/parent entity)))
                  (dolist (child (node/children entity))
                    (if reparent-children-p
                        (add-child child :parent parent)
                        (%delete child)))
                  (on-entity-deleted entity)
                  (a:deletef (node/children parent) entity))))
       (%delete entity)))))

(defgeneric on-update (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-render (entity)
  (:method-combination progn)
  (:method progn (entity)))
