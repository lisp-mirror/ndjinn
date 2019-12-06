(in-package #:pyx)

(define-component identify ()
  ((%identify/uuid :reader identify/uuid
                   :initform (make-uuid))
   (%identify/picking-id :reader identify/picking-id
                         :initform (generate-picking-id)))
  (:sorting :before node)
  (:static t))

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid))
    (u:href (uuids (current-scene *state*)) uuid))
  (:method ((uuid string))
    (u:href (uuids (current-scene *state*)) (string->uuid uuid))))

(defun find-by-picking-id (id)
  (u:href (picking-ids (current-scene *state*)) id))

(defun generate-picking-id ()
  (with-slots (%picking-ids %released-picking-ids) (current-scene *state*)
    (let ((id-count (hash-table-count %picking-ids)))
      (or (pop %released-picking-ids) id-count))))

(defun release-picking-id (id)
  (with-slots (%picking-ids %released-picking-ids) (current-scene *state*)
    (remhash id %picking-ids)
    (pushnew id %released-picking-ids)
    (setf %released-picking-ids (sort (copy-seq %released-picking-ids) #'<))))

(defmethod on-component-added (entity (component (eql 'identify)))
  (with-slots (%uuids %picking-ids) (current-scene *state*)
    (with-slots (%identify/uuid %identify/picking-id) entity
      (u:if-found (found (u:href %uuids %identify/uuid))
                  (error "Entity ~s has a UUID collision with object ~s."
                         entity found)
                  (setf (u:href %uuids %identify/uuid) entity))
      (setf (u:href %picking-ids %identify/picking-id) entity))))

(defmethod on-entity-deleted progn ((entity identify))
  (with-slots (%uuids) (current-scene *state*)
    (with-slots (%identify/uuid %identify/picking-id) entity
      (remhash %identify/uuid %uuids)
      (release-picking-id %identify/picking-id))))
