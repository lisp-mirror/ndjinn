(in-package #:pyx)

(defmacro make-entity ((&rest components) &body body)
  (a:with-gensyms (entity component-type)
    (let ((components (compute-component-order components)))
      `(let ((,entity (apply #'make-mixin ',components (list ,@body))))
         (dolist (,component-type ',components)
           (enqueue
            :entity-flow
            (list
             :component-add-hook
             (lambda ()
               (on-component-added ,entity ,component-type)))))
         ,entity))))

(defun modify-entity (entity &rest args)
  (enqueue
   :entity-flow
   (list :component-modify
         (lambda ()
           (apply #'reinitialize-instance entity :allow-other-keys t args)))))

(defgeneric on-update (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-render (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defmethod handle-queued-event ((purpose (eql :entity-flow)) event-type data)
  (case event-type
    ((:component-add :component-add-hook :component-modify :component-remove)
     (funcall data))
    (t (unhandled-queue-event-type purpose event-type))))
