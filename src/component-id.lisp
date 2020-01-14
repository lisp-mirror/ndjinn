(in-package #:pyx)

(define-component id ()
  ((%id/uuid :reader id/uuid
             :initform (make-uuid))
   (%id/display :reader id/display
                :initform "[Anonymous]"))
  (:sorting :after node)
  (:static t))

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid))
    (u:href (uuids (get-scene)) uuid))
  (:method ((uuid string))
    (u:href (uuids (get-scene)) (string->uuid uuid))))

;;; entity hooks

(define-hook :create (entity id)
  (with-slots (%uuids) (get-scene)
    (u:if-found (found (u:href %uuids id/uuid))
                (error "Entity ~s has a UUID collision with object ~s."
                       entity found)
                (setf (u:href %uuids id/uuid) entity))))

(define-hook :delete (entity id)
  (with-slots (%uuids) (get-scene)
    (remhash id/uuid %uuids)))
