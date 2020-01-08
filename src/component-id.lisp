(in-package #:pyx)

(define-component id ()
  ((%id/uuid :reader id/uuid
             :initform (make-uuid))
   (%id/picking-id :accessor id/picking-id
                   :initform nil))
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

;;; entity hooks

(define-hook :create (entity id)
  (with-slots (%uuids %picking-ids) (current-scene *state*)
    (u:if-found (found (u:href %uuids id/uuid))
                (error "Entity ~s has a UUID collision with object ~s."
                       entity found)
                (setf (u:href %uuids id/uuid) entity))
    (if (eq (node/pickable entity) :begin)
        (let ((id (generate-picking-id)))
          (setf id/picking-id id
                (u:href %picking-ids id) entity))
        (a:when-let ((parent (node/parent entity)))
          (unless (eq (node/pickable parent) :end)
            (setf id/picking-id (id/picking-id parent)))))))

(define-hook :delete (entity id)
  (with-slots (%uuids) (current-scene *state*)
    (remhash id/uuid %uuids)
    (release-picking-id id/picking-id)))
