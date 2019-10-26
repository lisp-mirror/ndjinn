(in-package #:pyx.web)

(let (site-path)
  (defun resolve-path (path)
    (flet ((get-path ()
             (uiop:merge-pathnames*
              #p"site/"
              (uiop:pathname-directory-pathname
               (if (and (boundp '*deployed-p*)
                        *deployed-p*)
                   #+sbcl
                   (first sb-ext:*posix-argv*)
                   #-sbcl
                   (error "Server was not deployed with SBCL.")
                   (uiop:pathname-parent-directory-pathname
                    #.(or *compile-file-truename* *load-truename*)))))))
      (uiop:merge-pathnames*
       path
       (or site-path (setf site-path (get-path)))))))

(defun get-time (&optional (as :int))
  (ecase as
    (:int (get-universal-time))
    (:ts (local-time:now))))

(defun time->int (value)
  (typecase value
    (local-time:timestamp (local-time:timestamp-to-universal value))
    (integer value)))

(defun int->time (value)
  (typecase value
    (integer (local-time:universal-to-timestamp value))
    (local-time:timestamp value)))

(defun int->bool (value)
  (ecase value
    (0 nil)
    (1 t)))

(defun time+ (time amount unit)
  (local-time:timestamp+ time amount unit))

(defun format-time (format &optional time)
  (let ((time (int->time (or time (get-time)))))
    (local-time:format-timestring nil time :format format)))

(defun bool->int (value)
  (if value 1 0))

(defun send-email (&key to subject message)
  (cl-smtp:send-email (cfg :smtp.host)
                      (cfg :smtp.from)
                      to
                      subject
                      message
                      :ssl (cfg :smtp.ssl)
                      :authentication
                      `(:login
                        ,(cfg :smtp.login)
                        ,(cfg :smtp.password))))

(defun extend-tag (form &key id class)
  (a:format-symbol :keyword "~a~@[#~a~]~@[.~a~]" form id class))
