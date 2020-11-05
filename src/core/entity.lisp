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

(defun register-entity (entity)
  (on-entity-create entity)
  (dolist (type (get-mixin-class-names entity))
    (when (has-component-p entity type)
      (on-entity-attach entity type))))

(defun %make-entity (types &optional args)
  (let ((class (make-entity-class types)))
    (apply #'make-instance class
           (when args (u:hash->plist args)))))

(defmacro make-entity ((&rest types) &body body)
  (u:with-gensyms (entity)
    `(let ((,entity (%make-entity ',(compute-component-type-order types)
                                  (u:plist->hash (list ,@body) :test #'eq))))
       (register-entity ,entity))))

(defgeneric %query-filter (entity))

(ff:define-filtered-function query (entity query)
  (:method (entity query)
    (error "Entity ~s has no defined query parameter: ~s." entity query))
  (:filters (:query (list #'identity (%query-filter entity)))))

(defun get-flow-hook-parameters (hook entity type data)
  (ecase hook
    ((:create :delete :physics-update :update :pre-render :render)
     `((,entity ,type)))
    ((:attach :detach)
     `(,entity (type (eql ',type))))
    ((:window-resize)
     `((,entity ,type) ,@`(&key ,@data)))))

(defgeneric on-entity-create (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-entity-delete (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-entity-physics-update (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-entity-update (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-entity-pre-render (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-entity-render (entity)
  (:method-combination progn :most-specific-last)
  (:method progn (entity)))

(defgeneric on-entity-attach (entity type)
  (:method-combination progn :most-specific-last)
  (:method progn (entity type)))

(defgeneric on-entity-detach (entity type)
  (:method-combination progn :most-specific-last)
  (:method progn (entity type)))

(defgeneric on-entity-window-resize (entity &key)
  (:method-combination progn :most-specific-last)
  (:method progn (entity &key old-size new-size)
    (declare (ignore old-size new-size))))

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

(defun find-parent (node type)
  (u:when-let ((parent (entity-parent node)))
    (if (has-component-p parent type)
        parent
        (find-parent parent type))))

(defun has-component-p (entity type)
  (and (find-class type nil)
       (typep entity type)))

(defun attach-component (entity type &rest args)
  (apply #'add-mixin-class entity type args)
  (on-entity-attach entity type))

(defun %detach-component (entity type &key (remove-mixin t))
  (if (find type =meta/component-static=)
      (error "Cannot remove built-in static component: ~s." type)
      (queue-flow-work 'detach
                       (lambda ()
                         (on-entity-detach entity type)
                         (when remove-mixin
                           (remove-mixin-class entity type))))))

(defun detach-component (entity type)
  (%detach-component entity type))

(defun %detach-components (entity &key (remove-mixins t))
  (dolist (component (get-mixin-class-names entity))
    (unless (find component =meta/component-static=)
      (%detach-component entity component :remove-mixin remove-mixins))))

(defun detach-components (entity)
  (%detach-components entity))

(defmacro define-entity-query-types (entity &body body)
  `(defmethod %query-filter ((entity ,entity))
     (lambda (query)
       (case query
         ,@(mapcar #'reverse body)))))

(defmacro define-entity-query ((entity component) (query parameter) &body body)
  `(defmethod query :filter :query
       ((,entity ,component) (,query (eql ',parameter)))
     ,@body))

(defmacro define-entity-hook (hook (entity type &rest data) &body body)
  (let ((method (u:format-symbol :net.mfiano.lisp.pyx "ON-ENTITY-~a" hook))
        (parameters (get-flow-hook-parameters hook entity type data)))
    `(defmethod ,method progn ,parameters
       ,@(when data
           `((declare (ignorable ,@data))))
       ,@body)))

(defun get-entity-count ()
  (hash-table-count (uuids (current-scene =context=))))

(defun invoke-entity-window-resize-hook (old-size new-size)
  (do-nodes (entity)
    (on-entity-window-resize entity :old-size old-size :new-size new-size)))
