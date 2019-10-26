(in-package #:pyx.web)

(defmethod validate ((type (eql :user-register)) &key email name password)
  (cond
    ((zerop (length email))
     (push-message :user-email-blank))
    ((not (valid-email-p email))
     (push-message :user-email-format-invalid :email email)))
  (when (find-user :email email)
    (push-message :user-email-unavailable :email email))
  (cond
    ((not (and (<= 3 (length name) 16)
               (every #'u:ascii-alphanumeric-p name)))
     (push-message :user-name-invalid-format))
    ((find-user :name name)
     (push-message :user-name-unavailable :name name)))
  (unless (>= (length password) 8)
    (push-message :user-password-length)))

(defmethod validate ((type (eql :user-activate)) &key email code)
  (a:if-let ((user (find-user :email email)))
    (let ((token (user-activation-token user)))
      (unless (string= (make-token-hash (token-time-created token) email code)
                       (token-hash token))
        (push-message :user-activate-incorrect-code)))
    (push-message :user-email-not-found :email email)))

(defmethod validate ((type (eql :user-activate-resend-code)) &key email)
  (let ((user (find-user :email email)))
    (cond
      ((not user)
       (push-message :user-email-not-found :email email))
      ((not (user-disabled-p user))
       (push-message :user-activate-previously-activated :email email)))))

(defmethod validate ((type (eql :user-sign-in)) &key name password)
  (let ((user (find-user :name name)))
    (cond
      ((and user (user-disabled-p user))
       (push-message :user-sign-in-not-activated))
      ((or (not user)
           (not (verify-password password (user-password user))))
       (push-message :user-sign-in-unauthorized)))))
