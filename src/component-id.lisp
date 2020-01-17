(in-package #:pyx)

(define-component id ()
  ((%id/uuid :reader id/uuid
             :initform (make-uuid))
   (%id/display :reader id/display
                :initform "[Anonymous]")
   (%id/views :accessor id/views
              :initarg :id/views
              :initform nil))
  (:sorting :after node)
  (:static t))

(defun id/register-uuid (entity)
  (with-slots (%uuids) (get-scene)
    (let ((uuid (id/uuid entity)))
      (u:if-found (found (u:href %uuids uuid))
                  (error "Entity ~s has a UUID collision with object ~s."
                         entity found)
                  (setf (u:href %uuids uuid) entity)))))

(defun id/deregister-uuid (entity)
  (remhash (id/uuid entity) (uuids (get-scene))))

(defun id/register-views (entity)
  (a:when-let ((parent (node/parent entity)))
    (dolist (id (id/views parent))
      (pushnew id (id/views entity))))
  (dolist (id (id/views entity))
    (pushnew entity (u:href (view-tags (get-scene)) id))))

(defun id/deregister-views (entity)
  (dolist (id (id/views entity))
    (a:deletef (u:href (view-tags (get-scene)) id) entity)))

;;; protocol

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid))
    (u:href (uuids (get-scene)) uuid))
  (:method ((uuid string))
    (u:href (uuids (get-scene)) (string->uuid uuid))))

;;; entity hooks

(define-hook :create (entity id)
  (id/register-uuid entity)
  (id/register-views entity))

(define-hook :delete (entity id)
  (id/deregister-uuid entity)
  (id/deregister-views entity))
