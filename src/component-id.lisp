(in-package #:%pyx.component.id)

(ent:define-component id ()
  ((%display :accessor display
             :initform "[Anonymous]")
   (%views :accessor views
           :initarg :id/views
           :initform nil)
   (%contact :reader contact
             :initarg :id/contact
             :initform nil)
   (%uuid :reader uuid
          :initform (util:make-uuid)))
  (:sorting :after c/node:node)
  (:static t))

(u:define-printer (ent:mixin stream :type nil)
  (format stream "~a" (display ent:mixin)))

(defun register-uuid (entity)
  (let ((uuids (scene:uuids (ctx:current-scene)))
        (uuid (uuid entity)))
    (u:if-found (found (u:href uuids uuid))
                (error "Entity ~s has a UUID collision with object ~s."
                       entity found)
                (setf (u:href uuids uuid) entity))))

(defun deregister-uuid (entity)
  (remhash (uuid entity) (scene:uuids (ctx:current-scene))))

(defun register-views (entity)
  (a:when-let ((parent (c/node:parent entity)))
    (dolist (id (views parent))
      (pushnew id (views entity)))))

(defun register-contact (entity)
  (a:when-let* ((scene (ctx:current-scene))
                (id (contact entity))
                (callback-entities (cd:callback-entities
                                    (scene:collision-system
                                     scene))))
    (unless (u:href callback-entities id)
      (setf (u:href callback-entities id) (u:dict #'eq)))
    (setf (u:href callback-entities id entity) entity)))

(defun deregister-contact (entity)
  (a:when-let* ((scene (ctx:current-scene))
                (id (contact entity))
                (callback-entities (cd:callback-entities
                                    (scene:collision-system
                                     scene))))
    (remhash entity (u:href callback-entities id))
    (when (zerop (hash-table-count (u:href callback-entities id)))
      (remhash id callback-entities))))

;;; protocol

(defgeneric find-by-uuid (uuid)
  (:method ((uuid util:uuid))
    (u:href (scene:uuids (ctx:current-scene)) uuid))
  (:method ((uuid string))
    (u:href (scene:uuids (ctx:current-scene)) (util:string->uuid uuid))))

;;; entity hooks

(ent:define-entity-hook :create (entity id)
  (register-uuid entity)
  (register-views entity)
  (register-contact entity))


(ent:define-entity-hook :delete (entity id)
  (deregister-uuid entity)
  (deregister-contact entity))
