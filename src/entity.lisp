(in-package #:pyx)

(defmacro make-entity ((&rest components) &body body)
  (a:with-gensyms (entity component-type)
    (let ((components (compute-component-order components)))
      `(let ((,entity (apply #'make-mixin ',components (list ,@body))))
         (dolist (,component-type ',components)
           (register-entity-flow-event
            :component-add-hook
            (lambda ()
              (on-component-added ,entity ,component-type))))
         ,entity))))

(defun register-entity-flow-event (event-type hook)
  (enqueue :entity-flow (list event-type hook)))

(defun modify-entity (entity &rest args)
  (register-entity-flow-event
   :component-modify
   (lambda ()
     (apply #'reinitialize-instance entity :allow-other-keys t args))))

(defun remove-entity (entity &key reparent-children-p)
  (register-entity-flow-event
   :entity-remove
   (lambda ()
     (let ((parent (node/parent entity)))
       (when reparent-children-p
         (dolist (child (node/children entity))
           (add-child child :parent parent)))
       (a:deletef (node/children parent) entity)))))

(defgeneric on-update (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-render (entity)
  (:method-combination progn)
  (:method progn (entity)))
