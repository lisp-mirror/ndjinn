(in-package #:pyx.web)

(define-model user ()
  (id :type integer :primary-key t)
  (name :type (:string 16) :unique t :not-null t)
  (password :type (:string 266) :unique t :not-null t)
  (email :type (:string 128) :unique t :not-null t)
  (activation-token :type integer :not-null t
                    :ref (token :id :on-delete :cascade))
  (login-token :type integer :ref (token :id :on-delete :cascade))
  (time-created :type integer :not-null t :func #'int->time)
  (time-sign-in :type integer :func #'int->time)
  (disabled-p :type boolean :func #'int->bool)
  (admin-p :type boolean :func #'int->bool))

(defun last-user-id ()
  (db:retrieve-one
   (sql:select :id
     (sql:from :user)
     (sql:order-by (:desc :id)))))

(defun insert-user (&key token name password email admin-p)
  (db:execute
   (sql:insert-into :user
     (sql:set= :name name
               :password password
               :email email
               :activation-token token
               :login-token 0
               :time-created (get-time)
               :disabled-p 1
               :admin-p (if admin-p 1 0)))))

(defun find-user (key value)
  (let ((valid-keys '(:id :name :email)))
    (unless (member key valid-keys)
      (error "Invalid user key: ~a. Must be one of: ~{~a~^ ~}"
             key
             valid-keys))
    (db:retrieve-one
     (sql:select :*
       (sql:from :user)
       (sql:where (:= key value)))
     :as 'user)))

(defun activate-user-by-email (email)
  (db:execute
   (sql:update :user
     (sql:set= :disabled-p 0
               :activation-token 0)
     (sql:where (:= :email email)))))
