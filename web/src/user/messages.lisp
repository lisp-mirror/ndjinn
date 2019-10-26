(in-package #:pyx.web)

(defmethod make-message ((type (eql :user-email-blank)) &key)
  (format nil "Email address cannot be blank."))

(defmethod make-message ((type (eql :user-email-invalid)) &key email)
  (format nil "Invalid email address: ~a." email))

(defmethod make-message ((type (eql :user-email-unavailable)) &key email)
  (format nil "Email address unavailable: ~a." email))

(defmethod make-message ((type (eql :user-email-not-found)) &key email)
  (format nil "Email address does not exist or is already activated: ~a."
          email))

(defmethod make-message ((type (eql :user-password-length)) &key)
  (format nil "Password must be be at least 8 characters."))

(defmethod make-message ((type (eql :user-name-invalid-format)) &key)
  (format nil "Display name must must be 3-16 alpha-numeric characters."))

(defmethod make-message ((type (eql :user-name-unavailable)) &key name)
  (format nil "Display name unavailable: ~a." name))

(defmethod make-message ((type (eql :user-token-not-created)) &key)
  (format nil "An error occured while creating your activation code. If this ~
               problem persists, please email us at: ~a"
          (cfg :site.admin)))

(defmethod make-message ((type (eql :user-activate-email)) &key code)
  (format nil "You are receiving this message because you or someone else ~
               registered for an account at ~a using this email address. If ~
               this was you, you can verify your email address by visiting the ~
               following address and entering in the activation code ~
               provided.~%~%~
               Visit: ~a/user/activate/~%~
               and enter the activation code: ~a~%~%~
               This activation code will expire in 48 hours."
          (cfg :site.url)
          (cfg :site.url)
          code))

(defmethod make-message ((type (eql :user-activate-code-sent)) &key)
  (format nil "An activation code has been emailed to the address you provided ~
               and should be arriving soon.~%~%~
               Please enter your email address and activation code:"))

(defmethod make-message ((type (eql :user-activate-incorrect-code)) &key)
  (format nil "Incorrect activation code for this account."))

(defmethod make-message ((type (eql :user-activate-previously-activated))
                         &key email)
  (format nil "The account ~a is already activated." email))

(defmethod make-message ((type (eql :user-activate-success)) &key)
  (format nil "Your account has been successfully activated. Please sign in."))

(defmethod make-message ((type (eql :user-sign-in-unauthorized)) &key)
  (format nil "Incorrect email address or password."))

(defmethod make-message ((type (eql :user-sign-in-not-activated)) &key)
  (format nil "This account has not yet been activated."))
