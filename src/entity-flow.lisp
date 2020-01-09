(in-package #:pyx)

(defun get-entity-flow-hook-parameters (hook entity type)
  (ecase hook
    ((:create :delete :update :pre-render :render)
     `((,entity ,type)))
    ((:attach :detach)
     `(,entity (type (eql ',type))))))

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

(defgeneric on-create (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-delete (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-update (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-pre-render (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-render (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-attach (entity type)
  (:method-combination progn :most-specific-last)
  (:method progn (entity type)))

(defgeneric on-detach (entity type)
  (:method-combination progn :most-specific-last)
  (:method progn (entity type)))
