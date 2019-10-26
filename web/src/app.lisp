(in-package #:pyx.web)

(defvar *app* nil)

(defclass app (web:<app>)
  ((%handler :reader handler
             :initform nil)))

(defun make-handler (thread-p)
  (if (and *app* (handler *app*))
      (error "The server is already running.")
      (setf (slot-value *app* '%handler)
            (clack:clackup
             (lack.builder:builder
              (add-middleware :access-log)
              (add-middleware :error-log)
              (add-middleware :database-log)
              (add-middleware :static)
              (add-middleware :session)
              (add-middleware :csrf)
              *app*)
             :server (cfg :server)
             :port (cfg :port)
             :use-thread thread-p))))

(defun start (&key (thread-p t))
  (load-config)
  (djula:add-template-directory (resolve-path "templates/"))
  (connect-database (cfg :db.type))
  (start-task-manager)
  (make-handler thread-p)
  (u:noop))

(defun stop ()
  (with-slots (%handler) *app*
    (if (and *app* %handler)
        (progn
          (disconnect-database)
          (stop-task-manager)
          (clack:stop %handler)
          (setf %handler nil)
          (u:noop))
        (error "Server has not been started yet."))))
