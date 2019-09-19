(in-package #:pyx)

(defmacro make-entity ((&rest components) &body body)
  (a:with-gensyms (entity)
    (let ((components (compute-component-order components)))
      `(let ((,entity (apply #'make-mixin ',components (list ,@body))))
         (dolist (name ',components)
           (on-component-added name ,entity))
         ,entity))))

(defgeneric on-update (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-render (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))
