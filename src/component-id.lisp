(in-package #:pyx)

(define-component id ()
  ((%id/uuid :reader id/uuid
             :initform (make-uuid))
   (%id/display :reader id/display
                :initform "[Anonymous]")
   (%id/tags :accessor id/tags
             :initarg :id/tags
             :initform nil))
  (:sorting :after node)
  (:static t))

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid))
    (u:href (uuids (get-scene)) uuid))
  (:method ((uuid string))
    (u:href (uuids (get-scene)) (string->uuid uuid))))

(defun find-by-tag (tag)
  (u:href (tags (get-scene)) tag))

;;; entity hooks

(define-hook :create (entity id)
  (with-slots (%uuids %tags) (get-scene)
    (u:if-found (found (u:href %uuids id/uuid))
                (error "Entity ~s has a UUID collision with object ~s."
                       entity found)
                (setf (u:href %uuids id/uuid) entity))
    (dolist (tag id/tags)
      (pushnew entity (u:href %tags tag)))))

(define-hook :delete (entity id)
  (with-slots (%uuids %tags) (get-scene)
    (remhash id/uuid %uuids)
    (dolist (tag id/tags)
      (a:deletef (u:href %tags tag) entity))))
