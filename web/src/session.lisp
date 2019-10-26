(in-package #:pyx.web)

(defun session-value (key)
  (u:href web:*session* key))

(defun (setf session-value) (value key)
  (setf (u:href web:*session* key) value))

(defun clear-session (&optional key)
  (if key
      (remhash key web:*session*)
      (clrhash web:*session*)))

(defun csrf-token ()
  (lack.middleware.csrf:csrf-token web:*session*))

(defun get-user-agent ()
  (let ((headers (caveman2:request-headers web:*request*)))
    (u:href headers "user-agent")))

(defun get-ip ()
  (let ((headers (web:request-headers web:*request*)))
    (or (u:href headers "x-forwarded-for")
        (web:request-remote-addr web:*request*))))
