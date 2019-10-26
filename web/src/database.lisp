(in-package #:pyx.web)

(defvar *database-tables* nil)

(defmacro with-transaction (() &body body)
  `(cl-dbi:with-transaction db:*connection*
     ,@body))

(defmacro with-tracing (() &body body)
  `(let ((db:*trace-sql* t))
     ,@body))

(defgeneric make-table (name))

(defun seed-database ()
  (with-transaction ()
    (db:execute (sql:pragma :foreign_keys 1))
    (dolist (table *database-tables*)
      (make-table table))))

(defgeneric connect-database (driver)
  (:method :around (driver)
    (symbol-macrolet ((connection db:*connection*))
      (when connection
        (error "Database already connected."))
      (setf connection (call-next-method))
      (seed-database)
      (u:noop))))

(defmethod connect-database ((driver (eql :sqlite)))
  (let ((file (resolve-path (cfg :db.name))))
    (ensure-directories-exist file)
    (dbi:connect :sqlite3 :database-name file)))

(defmethod connect-database ((driver (eql :postgresql)))
  (dbi:connect :postgres
               :database-name (cfg :db.name)
               :host (cfg :db.host)
               :port (cfg :db.port)
               :username (cfg :db.user)
               :password (cfg :db.password)))

(defun disconnect-database ()
  (db:disconnect-toplevel))

(defun get-row (query &key as)
  (db:retrieve-one query :as as))

(defun get-rows (query &key as)
  (db:retrieve-all query :as as))
