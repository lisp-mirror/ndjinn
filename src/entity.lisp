(in-package #:pyx)

(defclass entity () ())

(defmacro make-entity ((&rest components) &body body)
  (a:with-gensyms (mixin entity)
    (let ((components `(entity node xform ,@components)))
      `(let* ((,mixin (dynamic-mixins:mix
                       ,@(mapcar
                          (lambda (x) `',x)
                          components)))
              (,entity (apply #'make-instance ,mixin (list ,@body))))
         (dolist (name ',components)
           (on-component-added name ,entity))
         ,entity))))

(u:define-printer (entity stream :type nil)
  (format stream "ENTITY (~{~a~^, ~})" (cdr (get-components entity))))

(defgeneric on-update (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-render (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))
