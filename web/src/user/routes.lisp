(in-package #:pyx.web)

(define-route "/user/sign-in" :get ()
  (if (session-value :user-name)
      (web:redirect "/home")
      (render :user-sign-in)))

(define-route "/user/sign-in" :post (name password)
  (apply #'validate :user-sign-in @)
  (if (messages-p)
      (apply #'render :user-sign-in @)
      (let* ((user (find-user :name name))
             (name (user-name user)))
        (setf (session-value :user-id) (user-id user)
              (session-value :user-email) (user-email user)
              (session-value :user-name) name)
        (web:redirect (format nil "/user/profile/~a" name)))))

(define-route "/user/sign-out" :get ()
  (if (session-value :user-name)
      (progn
        (clear-session)
        (web:redirect "/home"))
      (web:redirect "/user/sign-in")))

(define-route "/user/register" :get ()
  (render :user-register))

(define-route "/user/register" :post (email name password)
  (apply #'validate :user-register @)
  (if (messages-p)
      (apply #'render :user-register @)
      (if (apply #'make-user-account @)
          (progn
            (push-message :user-activate-code-sent)
            (web:redirect "/user/activate"))
          (apply #'render :user-register @))))

(define-route "/user/activate" :get ()
  (render :user-activate))

(define-route "/user/activate" :post (email code)
  (apply #'validate :user-activate @)
  (if (messages-p)
      (apply #'render :user-activate @)
      (progn
        (activate-user-by-email email)
        (push-message :user-activate-success)
        (web:redirect "/user/sign-in"))))

(define-route "/user/activate/resend-code" :get ()
  (render :user-activate=resend-code))

(define-route "/user/activate/resend-code" :post (email)
  (validate :user-activate-resend-code :email email)
  (if (messages-p)
      (render :user-activate-resend-code)
      (progn
        (resend-activation-code email)
        (web:redirect "/user/activate"))))

(define-route "/user/profile/:name" :get (name)
  (a:if-let ((user (find-user :name name)))
    (render :user-profile :name (user-name user))
    (render :404)))

(define-route "/user/settings" :get ()
  (if (session-value :user-name)
      (render :user-settings)
      (web:redirect "/user/sign-in")))
