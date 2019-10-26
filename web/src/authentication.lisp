(in-package #:pyx.web)

(defun make-salt (&key size)
  (let ((*random-state* (make-random-state t)))
    (ironclad:make-random-salt (or size 64))))

(defun hash-password (password &key salt rounds (digest :sha512))
  (let ((salt (or salt (make-salt)))
        (rounds (or rounds (cfg :auth.rounds))))
    (format nil "~8,'0d:~a:~a"
            rounds
            (ironclad:byte-array-to-hex-string salt)
            (ironclad:byte-array-to-hex-string
             (ironclad:pbkdf2-hash-password
              (trivial-utf-8:string-to-utf-8-bytes
               (concatenate 'string password (cfg :auth.secret)))
              :digest (or digest (cfg :auth.digest))
              :salt salt
              :iterations rounds)))))

(defun unpack-salt/hash (salt/hash)
  (destructuring-bind (rounds salt hash) (cl-ppcre:split "\\:+" salt/hash)
    (values hash
            (ironclad:hex-string-to-byte-array salt)
            (parse-integer rounds))))

(defun verify-password (password salt/hash)
  (u:mvlet* ((hash salt rounds (unpack-salt/hash salt/hash)))
    (string= (unpack-salt/hash
              (hash-password password
                             :salt salt
                             :rounds rounds
                             :digest (cfg :auth.digest)))
             hash)))
