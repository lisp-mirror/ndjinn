(in-package #:pyx.web)

(defun ensure-message-queue ()
  (unless (session-value :messages)
    (setf (session-value :messages) (lparallel.queue:make-queue))))

(defun push-message (message &rest args)
  (ensure-message-queue)
  (lparallel.queue:push-queue
   (etypecase message
     (keyword (apply #'make-message message args))
     (function (apply message args))
     (string message))
   (session-value :messages))
  (u:noop))

(defun get-messages ()
  (ensure-message-queue)
  (let ((queue (session-value :messages))
        (channel (lparallel:make-channel)))
    (lparallel:submit-task
     channel
     (lambda ()
       (loop :until (lparallel.queue:queue-empty-p queue)
             :collect (lparallel.queue:pop-queue queue))))
    (lparallel:receive-result channel)))

(defun messages-p ()
  (let ((queue (session-value :messages)))
    (not (lparallel.queue:queue-empty-p queue))))

(defgeneric make-message (type &key &allow-other-keys))

(defmethod make-message ((type (eql :unknown)) &key)
  (format nil "An unknown error has occured while handling your request. If ~
               this problem persists, please email us at: ~a"
          (cfg :site-admin)))
