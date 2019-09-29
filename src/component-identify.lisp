(in-package #:pyx)

(define-component identify (:before node)
  (:uuid (make-uuid)))

(defmethod on-component-added ((component (eql 'identify)) entity)
  (let ((uuid (identify/uuid entity))
        (uuids (uuid (database *state*))))
    (u:if-found (found (u:href uuids uuid))
                (error "Entity ~s has a UUID collision with object ~s."
                       entity found)
                (setf (u:href uuids uuid) entity))))

(defmethod on-component-removed ((component (eql 'identify)) entity)
  ;; TODO: ensure we remove the UUID when an entity is deleted, not just the
  ;; identify component.
  (let ((uuids (uuid (database *state*))))
    (remhash (identify/uuid entity) uuids)))
