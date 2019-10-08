(in-package #:pyx)

(define-component identify (:before node)
  (:uuid (make-uuid)))

(defmethod on-component-added (entity (component (eql 'identify)))
  (let ((uuid (identify/uuid entity))
        (uuids (uuid (database *state*))))
    (u:if-found (found (u:href uuids uuid))
                (error "Entity ~s has a UUID collision with object ~s."
                       entity found)
                (setf (u:href uuids uuid) entity))))

(defmethod on-component-removed (entity (component (eql 'identify)))
  ;; TODO: ensure we remove the UUID when an entity is deleted, not just the
  ;; identify component.
  (let ((uuids (uuid (database *state*))))
    (remhash (identify/uuid entity) uuids)))
