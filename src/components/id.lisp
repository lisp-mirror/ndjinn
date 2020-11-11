(in-package #:ndjinn)

(define-component id ()
  ((%id/display :accessor id/display
                :initarg :id/display
                :initform "[Anonymous]")
   (%id/views :accessor id/views
              :initarg :id/views
              :initform nil)
   (%id/uuid :reader id/uuid
             :initform (uuid:make-uuid)))
  (:type-order :after node :before transform)
  (:static t))

(u:define-printer (mixin stream :type nil)
  (format stream "~a" (id/display mixin)))

(defun register-uuid (entity)
  (let ((uuids (uuids (current-scene =context=)))
        (uuid (id/uuid entity)))
    (u:if-found (found (u:href uuids uuid))
      (error "Entity ~s has a UUID collision with object ~s."
             entity found)
      (setf (u:href uuids uuid) entity))))

(defun deregister-uuid (entity)
  (remhash (id/uuid entity) (uuids (current-scene =context=))))

(defun register-views (entity)
  (u:when-let ((parent (node/parent entity)))
    (dolist (id (id/views parent))
      (pushnew id (id/views entity)))))

;;; protocol

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid:uuid))
    (u:href (uuids (current-scene =context=)) uuid))
  (:method ((uuid string))
    (u:href (uuids (current-scene =context=)) (uuid:string->uuid uuid))))

(defun get-display-id (entity)
  (id/display entity))

(defun get-uuid (entity)
  (uuid:uuid->string (id/uuid entity)))

;;; entity hooks

(define-entity-hook :create (entity id)
  (register-uuid entity)
  (register-views entity))

(define-entity-hook :delete (entity id)
  (deregister-uuid entity))
