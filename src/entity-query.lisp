(in-package #:pyx)

(ff:define-filtered-function %entity-query (entity query)
  (:method (entity query)
    (error "Entity ~s has no defined query parameter: ~s." entity query))
  (:filters (:query (list #'identity (%entity-filter entity)))))

(defgeneric %entity-filter (entity))

(defmacro define-query-types (entity &body body)
  `(defmethod %entity-filter ((entity ,entity))
     (lambda (query)
       (case query
         ,@(mapcar #'reverse body)))))

(defmacro define-query ((entity component) (query parameter) &body body)
  `(defmethod %entity-query :filter :query
       ((,entity ,component) (,query (eql ',parameter)))
     ,@body))
