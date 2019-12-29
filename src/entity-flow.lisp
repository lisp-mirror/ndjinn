(in-package #:pyx)

(defun get-entity-flow-hook-parameters (hook entity type)
  (let ((hook-type (a:make-keyword
                    (first
                     (uiop:split-string (symbol-name hook)
                                        :separator '(#\-))))))
    (ecase hook-type
      (:entity `((,entity ,type)))
      (:component `(,entity (type (eql ',type)))))))

(defmacro define-hook (hook (entity type) &body body)
  (let ((method (a:format-symbol :pyx "ON-~a" hook))
        (parameters (get-entity-flow-hook-parameters hook entity type))
        (accessors (mapcar
                    (lambda (x)
                      (list (a:symbolicate x) x))
                    (compute-component-accessors type))))
    `(defmethod ,method progn ,parameters
       (with-accessors ,accessors ,entity
         ,@body))))

;;; entity flow event hooks

(defgeneric on-entity-create (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-entity-delete (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-entity-update (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-entity-render (entity)
  (:method-combination progn)
  (:method progn (entity)))

(defgeneric on-component-attach (entity type)
  (:method-combination progn)
  (:method progn (entity type)))

(defgeneric on-component-detach (entity type)
  (:method-combination progn)
  (:method progn (entity type)))
