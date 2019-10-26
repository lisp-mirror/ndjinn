(in-package #:pyx.web)

(defun make-token-hash (time email code)
  (let* ((secret (trivial-utf-8:string-to-utf-8-bytes (cfg :auth.secret)))
         (hmac (ironclad:make-hmac secret (cfg :auth.digest)))
         (time-data (trivial-utf-8:string-to-utf-8-bytes
                     (format nil "~a" (time->int time))))
         (email-data (trivial-utf-8:string-to-utf-8-bytes email))
         (code-data (trivial-utf-8:string-to-utf-8-bytes code)))
    (ironclad:update-hmac hmac time-data)
    (ironclad:update-hmac hmac email-data)
    (ironclad:update-hmac hmac code-data)
    (ironclad:byte-array-to-hex-string
     (ironclad:hmac-digest hmac))))

(defun make-activation-code (time email)
  (let ((code (string-upcase
               (ironclad:byte-array-to-hex-string
                (make-salt :size 4)))))
    (values
     code
     (make-token-hash time email code))))
