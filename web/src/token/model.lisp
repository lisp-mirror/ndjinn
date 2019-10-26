(in-package #:pyx.web)

(define-model token ()
  (id :type integer :primary-key t)
  (hash :type (:string 128) :unique t :not-null t)
  (time-created :type integer :not-null t :func #'int->time)
  (time-expires :type integer :not-null t :func #'int->time))

(defun insert-token (&key time hash)
  (db:execute
   (sql:insert-into :token
     (sql:set= :hash hash
               :time-created (time->int time)
               :time-expires (time->int (time+ (get-time :ts) 2 :day))))))

(defun find-token (key value)
  (let ((valid-keys '(:id :hash)))
    (unless (member key valid-keys)
      (error "Invalid token key: ~a. Must be one of: ~{~a~^ ~}"
             key
             valid-keys))
    (db:retrieve-one
     (sql:select :*
       (sql:from :token)
       (sql:where (:= key value)))
     :as 'token)))

(defun update-token-hash (id time hash)
  (db:execute
   (sql:update :token
     (sql:set= :hash hash
               :time-created (time->int time)
               :time-expires (time->int (time+ (get-time :ts) 2 :day)))
     (sql:where (:= :id id)))))

(defun delete-expired-tokens ()
  (db:execute
   (sql:delete-from :token
     (sql:where (:<= :time-expires (get-time))))))
