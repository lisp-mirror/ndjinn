(in-package #:pyx)

(define-component/static identify (:before node)
  (:uuid (make-uuid)
   :picking-id nil
   :prefab nil))

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid))
    (u:href (uuids (database *state*)) uuid))
  (:method ((uuid string))
    (u:href (uuids (database *state*)) (string->uuid uuid))))

(defun find-by-picking-id (id)
  (u:href (picking-id (database *state*)) id))

(defun generate-picking-id ()
  (with-slots (%database) *state*
    (let* ((table (picking-id %database))
           (id-count (hash-table-count table)))
      (if (zerop id-count)
          0
          (or (pop (released-picking-ids %database))
              id-count)))))

(defun release-picking-id (id)
  (with-slots (%picking-ids %released-picking-ids) (database *state*)
    (remhash id %picking-ids)
    (pushnew id %released-picking-ids)
    (setf %released-picking-ids (sort (copy-seq %released-picking-ids) #'<))))

(defmethod on-component-added (entity (component (eql 'identify)))
  (with-slots (%uuids %picking-ids) (database *state*)
    (with-slots (%identify/uuid %identify/picking-id) entity
      (u:if-found (found (u:href %uuids %identify/uuid))
                  (error "Entity ~s has a UUID collision with object ~s."
                         entity found)
                  (setf (u:href %uuids %identify/uuid) entity))
      (setf %identify/picking-id (generate-picking-id)
            (u:href %picking-ids %identify/picking-id) entity))))

(defmethod on-entity-deleted progn ((entity identify))
  (with-slots (%uuids) (database *state*)
    (with-slots (%identify/uuid %identify/picking-id) entity
      (remhash %identify/uuid %uuids)
      (release-picking-id %identify/picking-id))))
