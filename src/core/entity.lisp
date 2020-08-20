(in-package #:net.mfiano.lisp.pyx)

(defun get-entity-slots (types)
  (mapcan
   (lambda (x)
     (mapcar
      #'c2mop:slot-definition-name
      (c2mop:class-slots (find-class x))))
   types))

(defun make-entity-class (components)
  (make-mixin-class (make-mixin-class-list components)))

(defun register-entity (entity types)
  (on-create entity)
  (dolist (type types)
    (on-attach entity type)))

(defun %make-entity (types &optional args)
  (let* ((class (make-entity-class types))
         (entity (apply #'make-instance class
                        (when args (u:hash->plist args)))))
    (register-entity entity types)
    entity))

(defgeneric %query-filter (entity))

(ff:define-filtered-function query (entity query)
  (:method (entity query)
    (error "Entity ~s has no defined query parameter: ~s." entity query))
  (:filters (:query (list #'identity (%query-filter entity)))))

(defun get-flow-hook-parameters (hook entity type)
  (ecase hook
    ((:create :delete :physics-update :update :pre-render :render)
     `((,entity ,type)))
    ((:attach :detach)
     `(,entity (type (eql ',type))))))

(defgeneric on-create (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-delete (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-physics-update (entity)
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

(defmacro make-entity ((&rest components) &body body)
  (let ((components (compute-component-type-order components)))
    `(%make-entity ',components (u:plist->hash (list ,@body) :test #'eq))))

(defmacro do-nodes ((entity &key parent) &body body)
  `(map-nodes (lambda (,entity) ,@body) ,parent))

(defun delete-entity (entity &key reparent-children)
  (when (node/root-p entity)
    (error "Cannot remove the root entity."))
  (delete-node entity :reparent-children reparent-children))

(defun entity-parent (entity)
  (node/parent entity))

(defun entity-children (entity)
  (node/children entity))

(defun has-component-p (entity type)
  (and (find-class type nil)
       (typep entity type)))

(defun attach-component (entity type &rest args)
  (apply #'add-mixin-class entity type args)
  (on-attach entity type))

(defun detach-component (entity type)
  (if (find type =static-components=)
      (error "Cannot remove built-in static component: ~s." type)
      (progn
        (on-detach entity type)
        (remove-mixin-class entity type))))

(defun detach-components (entity)
  (dolist (component (get-mixin-class-names entity))
    (unless (find component =static-components=)
      (detach-component entity component))))

(defmacro define-entity-query-types (entity &body body)
  `(defmethod %query-filter ((entity ,entity))
     (lambda (query)
       (case query
         ,@(mapcar #'reverse body)))))

(defmacro define-entity-query ((entity component) (query parameter) &body body)
  `(defmethod query :filter :query
       ((,entity ,component) (,query (eql ',parameter)))
     ,@body))

(defmacro define-entity-hook (hook (entity type) &body body)
  (let ((method (u:format-symbol :net.mfiano.lisp.pyx "ON-~a" hook))
        (parameters (get-flow-hook-parameters hook entity type))
        (accessors (mapcar
                    (lambda (x)
                      (list (u:symbolicate x) x))
                    (compute-component-accessors type))))
    `(defmethod ,method progn ,parameters
       (with-accessors ,accessors ,entity
         ,@body))))

(defun get-entity-count ()
  (hash-table-count (uuids (current-scene))))
