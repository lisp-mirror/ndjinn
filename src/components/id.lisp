(in-package #:pyx.component)

(pyx:define-component id ()
  ((%id/display :accessor id/display
                :initform "[Anonymous]")
   (%id/views :accessor id/views
              :initarg :id/views
              :initform nil)
   (%id/contact :reader id/contact
                :initarg :id/contact
                :initform nil)
   (%id/uuid :reader id/uuid
             :initform (pyx::make-uuid)))
  (:sorting :after node)
  (:static t))

(u:define-printer (pyx::mixin stream :type nil)
  (format stream "~a" (id/display pyx::mixin)))

(defun register-uuid (entity)
  (let ((uuids (pyx::uuids (pyx::current-scene)))
        (uuid (id/uuid entity)))
    (u:if-found (found (u:href uuids uuid))
                (error "Entity ~s has a UUID collision with object ~s."
                       entity found)
                (setf (u:href uuids uuid) entity))))

(defun deregister-uuid (entity)
  (remhash (id/uuid entity) (pyx::uuids (pyx::current-scene))))

(defun register-views (entity)
  (a:when-let ((parent (node/parent entity)))
    (dolist (id (id/views parent))
      (pushnew id (id/views entity)))))

(defun register-contact (entity)
  (a:when-let* ((scene (pyx::current-scene))
                (id (id/contact entity))
                (callback-entities (pyx::callback-entities
                                    (pyx::collision-system scene))))
    (unless (u:href callback-entities id)
      (setf (u:href callback-entities id) (u:dict #'eq)))
    (setf (u:href callback-entities id entity) entity)))

(defun deregister-contact (entity)
  (a:when-let* ((scene (pyx::current-scene))
                (id (id/contact entity))
                (callback-entities (pyx::callback-entities
                                    (pyx::collision-system scene))))
    (remhash entity (u:href callback-entities id))
    (when (zerop (hash-table-count (u:href callback-entities id)))
      (remhash id callback-entities))))

;;; protocol

(defgeneric find-by-uuid (uuid)
  (:method ((uuid pyx::uuid))
    (u:href (pyx::uuids (pyx::current-scene)) uuid))
  (:method ((uuid string))
    (u:href (pyx::uuids (pyx::current-scene)) (pyx::string->uuid uuid))))

;;; entity hooks

(pyx:define-entity-hook :create (entity id)
  (register-uuid entity)
  (register-views entity)
  (register-contact entity))


(pyx:define-entity-hook :delete (entity id)
  (deregister-uuid entity)
  (deregister-contact entity))
