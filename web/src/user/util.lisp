(in-package #:pyx.web)

(defun valid-email-p (value)
  (let ((pattern (format nil "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~~-]+@[a-zA-Z0-9-]+~
                            (?:\\.[a-zA-Z0-9-]+)*$")))
    (when (ppcre:scan pattern value)
      t)))

(defun send-activation-code (email code)
  (run-task #'send-email
            :to email
            :subject "Welcome to lisp.cl"
            :message (make-message :user-activate-email :code code)))

(defun make-user-account (&key email password name)
  (u:mvlet* ((time (get-time :ts))
             (code hash (make-activation-code time email)))
    (with-transaction ()
      (insert-token :time time :hash hash)
      (a:if-let ((token (find-token :hash hash)))
        (progn
          (insert-user :token (token-id token)
                       :email email
                       :password (hash-password password)
                       :name name)
          (send-activation-code email code))
        (push-message :user-token-not-created)))))

(defun resend-activation-code (email)
  (u:mvlet* ((user (find-user :email email))
             (time (get-time :ts))
             (code hash (make-activation-code time email)))
    (update-token-hash (token-id (user-activation-token user)) time hash)
    (send-activation-code email code)))
