(in-package #:pyx)

(defmacro make-entity ((&rest components) &body body)
  (a:with-gensyms (entity)
    (let ((components (compute-component-order components)))
      `(let ((,entity (apply #'make-mixin ',components (list ,@body))))
         (dolist (name ',components)
           (on-component-added name ,entity))
         ,entity))))

(defun make-entity2 ()
  (let ((components (component-component-order)))))

(defun modify-entity (entity &rest args)
  (apply #'reinitialize-instance entity args))

(defgeneric on-update (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-render (entity)
  (:method-combination progn)
  (:method progn (entity)))
