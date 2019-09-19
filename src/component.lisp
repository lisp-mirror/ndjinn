(in-package #:pyx)

(defmacro define-component (type &body body)
  `(u:eval-always
     (defclass ,type ()
       ,(loop :for (key value) :on (car body) :by #'cddr
              :for accessor = (a:symbolicate type '#:/ key)
              :for slot = (a:symbolicate '#:% accessor)
              :for initarg = (a:make-keyword accessor)
              :collect `(,slot :accessor ,accessor
                               :initarg ,initarg
                               :initform ,value)))
     (defmethod has-component-p ((type (eql ',type)) entity)
       (typep entity ',type))))

(defun get-components (entity)
  (cdr
   (mapcar #'class-name
           (c2mop:class-direct-superclasses (class-of entity)))))

(defgeneric on-component-added (component entity)
  (:method (component entity)))

(defgeneric on-component-removed (component entity)
  (:method (component entity)))

(defun add-component (entity component &rest args)
  (let* ((current (append (get-components entity) (list component)))
         (new-class (dynamic-mixins::ensure-mixin
                     (apply #'dynamic-mixins:mix current))))
    (change-class entity new-class)
    (apply #'reinitialize-instance entity args)
    (on-component-added component entity)
    entity))

(defun remove-component (entity component)
  (let ((new-entity (dynamic-mixins:delete-from-mix entity component)))
    (on-component-removed component new-entity)
    new-entity))
