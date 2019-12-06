(in-package #:pyx)

(define-component group ()
  ((%group/name :reader group/name
                :initarg :group/name
                :initform nil))
  (:sorting :before node)
  (:static t))

(defmethod on-component-added (entity (component (eql 'group)))
  (with-slots (%node/parent %group/name) entity
    (let ((group-name (or %group/name (group/name %node/parent) 'default)))
      (group-join entity group-name))))

(defmethod on-entity-deleted progn ((entity group))
  (group-leave entity (group/name entity)))

(defun check-group (group-name)
  (unless (meta :groups group-name)
    (error "Group ~s does not exist." group-name)))

(defun group-join (entity group-name)
  (with-slots (%group/name) entity
    (let ((group-name (or group-name 'default)))
      (symbol-macrolet ((entities (u:href (groups (database *state*))
                                          group-name)))
        (check-group group-name)
        (group-leave entity %group/name)
        (unless (find entity entities)
          (a:appendf entities (list entity)))
        (setf %group/name group-name)
        (insert-draw-order-group group-name)
        (sort-group-entities)))))

(defun group-leave (entity group-name)
  (with-slots (%group/name) entity
    (let ((group-name (or group-name 'default)))
      (check-group group-name)
      (a:deletef (u:href (groups (database *state*)) group-name) entity)
      (a:deletef (draw-order-entities (database *state*)) entity)
      (setf %group/name 'default)
      (sort-group-entities))))

(defun insert-draw-order-group (group-name)
  (with-slots (%draw-order-groups) (database *state*)
    (unless (find group-name %draw-order-groups)
      (let ((groups (cons group-name (copy-seq %draw-order-groups))))
        (setf %draw-order-groups
              (sort groups #'<
                    :key (lambda (x) (draw-order (meta :groups x)))))))))

(defun sort-group-entities ()
  (with-slots (%groups %draw-order-groups %draw-order-entities)
      (database *state*)
    (let (entities)
      (dolist (group-name %draw-order-groups)
        (dolist (entity (u:href %groups group-name))
          (when (has-component-p entity 'render)
            (push entity entities))))
      (setf %draw-order-entities (nreverse entities)))))
