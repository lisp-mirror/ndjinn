(in-package #:pyx)

(define-component identify (:before node)
  (:uuid (make-uuid)
   :picking-id nil
   :prefab nil))

(defmethod on-component-added (entity (component (eql 'identify)))
  (with-slots (%uuids %picking-ids) (database *state*)
    (with-slots (%identify/uuid %identify/picking-id) entity
      (u:if-found (found (u:href %uuids %identify/uuid))
                  (error "Entity ~s has a UUID collision with object ~s."
                         entity found)
                  (setf (u:href %uuids %identify/uuid) entity))
      (setf %identify/picking-id (generate-picking-id)
            (u:href %picking-ids %identify/picking-id) entity))))

(defmethod on-component-removed (entity (component (eql 'identify)))
  (with-slots (%uuids %picking-ids %released-picking-ids) (database *state*)
    (with-slots (%identify/uuid %identify/picking-id) entity
      (remhash %identify/uuid %uuids)
      (remhash %identify/picking-id %picking-ids)
      (push %identify/picking-id %released-picking-ids))))
