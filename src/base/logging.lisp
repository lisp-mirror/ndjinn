(in-package #:net.mfiano.lisp.pyx)

(defun start-logging ()
  (unless (log:thread log:*global-controller*)
    (log:start log:*global-controller*))
  (setf (log:repl-level) =log-repl-level=
        (log:repl-categories) =log-repl-categories=)
  (u:when-let* ((pool (find-asset-pool =log-assets=))
                (pool-path (resolve-path pool))
                (debug-log (uiop:merge-pathnames*
                            (make-pathname :name "debug" :type "log")
                            pool-path))
                (error-log (uiop:merge-pathnames*
                            (make-pathname :name "error" :type "log")
                            pool-path)))
    (ensure-directories-exist debug-log)
    (ensure-directories-exist error-log)
    (log:define-pipe ()
      (log:level-filter :level :debug)
      (log:rotating-file-faucet :template debug-log))
    (log:define-pipe ()
      (log:level-filter :level :error)
      (log:rotating-file-faucet :template error-log))))

(defun stop-logging ()
  (log:sync))
