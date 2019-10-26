(in-package #:pyx.web)

(defun run-task (task &rest args)
  (let ((channel (lparallel:make-channel)))
    (lparallel:submit-task
     channel
     (let ((session caveman2:*session*))
       (lambda ()
         (let ((caveman2:*session* session))
           (apply task args)))))
    channel))

(defun start-task-manager ()
  (setf lparallel:*kernel* (lparallel:make-kernel (cfg :task.threads)))
  (run-task
   (lambda ()
     (u:while t
       (sleep (* 60 (cfg :task.interval)))
       (with-transaction ()
         (run-task #'delete-expired-tokens))))))

(defun stop-task-manager ()
  (lparallel:end-kernel))
