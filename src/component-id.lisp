(in-package #:pyx)

(define-component id ()
  ((%id/uuid :reader id/uuid
             :initform (make-uuid))
   (%id/display :reader id/display
                :initform "[Anonymous]")
   (%id/views :accessor id/views
              :initarg :id/views
              :initform nil)
   (%id/contact :reader id/contact
                :initarg :id/contact
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
    (pushnew entity (u:href (tags (viewports (get-scene))) id))))

(defun id/deregister-views (entity)
  (let ((tags (tags (viewports (get-scene)))))
    (dolist (id (id/views entity))
      (a:deletef (u:href tags id) entity)
      (unless (u:href tags id)
        (remhash id tags)))))

(defun id/register-contact (entity)
  (with-slots (%callback-entities) (collision-system (get-scene))
    (a:when-let ((id (id/contact entity)))
      (unless (u:href %callback-entities id)
        (setf (u:href %callback-entities id) (u:dict #'eq)))
      (setf (u:href %callback-entities id entity) entity))))

(defun id/deregister-contact (entity)
  (with-slots (%callback-entities) (collision-system (get-scene))
    (a:when-let ((id (id/contact entity)))
      (remhash entity (u:href %callback-entities id))
      (when (zerop (hash-table-count (u:href %callback-entities id)))
        (remhash id %callback-entities)))))

;;; protocol

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid))
    (u:href (uuids (get-scene)) uuid))
  (:method ((uuid string))
    (u:href (uuids (get-scene)) (string->uuid uuid))))

;;; entity hooks

(define-hook :create (entity id)
  (id/register-uuid entity)
  (id/register-views entity)
  (id/register-contact entity))


(define-hook :delete (entity id)
  (id/deregister-uuid entity)
  (id/deregister-views entity)
  (id/deregister-contact entity))
