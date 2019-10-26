(in-package #:pyx.web)

(defgeneric add-middleware (type)
  (:method (type)
    type))

(defmethod add-middleware ((type (eql :access-log)))
  (a:when-let ((file (cfg :path.log.access)))
    (flet ((log-line (line)
             (with-open-file (out (resolve-path file)
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
               (format out "~&~a~%" line))))
      (setf lack.middleware.accesslog:*time-format*
            '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)
              ":" (:sec 2)))
      (list :accesslog :logger #'log-line))))

(defmethod add-middleware ((type (eql :error-log)))
  (a:when-let ((file (cfg :path.log.error)))
    (list :backtrace :output (resolve-path file))))

(defmethod add-middleware ((type (eql :database-log)))
  (when (cfg :db.debug)
    (lambda (app)
      (lambda (env)
        (with-tracing ()
          (funcall app env))))))

(defmethod add-middleware ((type (eql :static)))
  (flet ((static-p (path)
           (let ((pattern "^(?:/images/|/css/|/fonts/|/js/|/favicon\\.png$)"))
             (when (ppcre:scan pattern path)
               path))))
    (a:when-let ((path (cfg :path.static)))
      (list :static
            :path #'static-p
            :root (resolve-path path)))))
