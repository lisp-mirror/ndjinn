(in-package #:pyx)

(defgeneric %entity-filter (entity))

(defmacro define-query-types (entity &body body)
  `(defmethod %entity-filter ((entity ,entity))
     (lambda (query)
       (case query
         ,@(mapcar #'reverse body)))))

(defmacro define-query ((entity component) (query parameter) &body body)
  `(defmethod %entity-query :filter :query
       ((,query (eql ',parameter)) (,entity ,component))
     ,@body))

(ff:define-filtered-function %entity-query (query entity)
  (:method (query entity)
    (error "Entity ~s has no defined query parameter: ~s." entity query))
  (:filters (:query (%entity-filter entity))))
