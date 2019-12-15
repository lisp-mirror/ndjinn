(in-package #:pyx)

(defvar *thread-pool*)

(defclass thread-pool ()
  ((%worker-count :reader worker-count
                  :initarg :worker-count)
   (%channels :reader channels
              :initform (u:dict #'eq))
   (%queues :reader queues
            :initform (u:dict #'eq))))

(defun make-thread-pool ()
  (let* ((worker-count (cfg :threads))
         (thread-pool (make-instance 'thread-pool :worker-count worker-count)))
    (setf lparallel:*kernel* (lparallel:make-kernel worker-count)
          *thread-pool* thread-pool)))

(defun destroy-thread-pool ()
  (lparallel:end-kernel :wait t)
  (when (and (boundp '*thread-pool*) *thread-pool*)
    (setf *thread-pool* nil
          lparallel:*kernel* nil)))

(defun ensure-channel (purpose)
  (let ((channels (channels *thread-pool*)))
    (a:ensure-gethash purpose channels (lparallel:make-channel))))

(defun ensure-queue (purpose)
  (let ((queues (queues *thread-pool*)))
    (a:ensure-gethash purpose queues (lparallel.queue:make-queue))))

(defun submit-job (purpose job &optional (priority :default))
  (when (and (boundp '*thread-pool*) *thread-pool*)
    (let ((channel (ensure-channel purpose))
          (lparallel:*task-priority* priority)
          (lparallel:*task-category* purpose))
      (lparallel:submit-task channel job))))

(defun get-job-results (purpose)
  (when (and (boundp '*thread-pool*) *thread-pool*)
    (let ((channel (ensure-channel purpose)))
      (lparallel:receive-result channel))))

(defun kill-jobs (purpose)
  (lparallel:kill-tasks purpose))

(defun enqueue (purpose data)
  (when (and (boundp '*thread-pool*) *thread-pool*)
    (let ((queue (ensure-queue purpose)))
      (lparallel.queue:push-queue data queue))))

(defun dequeue (purpose)
  (when (and (boundp '*thread-pool*) *thread-pool*)
    (let ((queue (ensure-queue purpose)))
      (lparallel.queue:pop-queue queue))))

(defun queue-empty-p (purpose)
  (let ((queue (ensure-queue purpose)))
    (lparallel.queue:queue-empty-p queue)))

(defun process-queue (purpose)
  (u:while (and (boundp '*thread-pool*)
                *thread-pool*
                (not (queue-empty-p purpose)))
    (destructuring-bind (&optional event-type data) (dequeue purpose)
      (when event-type
        (handle-queued-event purpose event-type data)))))

(defgeneric handle-queued-event (purpose event-type data)
  (:method (purpose event-type data)
    (error "Unhandled queue event type ~s for queue purpose ~a."
           event-type purpose)))

(defmacro define-event-handler (purpose event-type &optional func)
  `(defmethod handle-queued-event ((purpose (eql ,purpose))
                                   (event-type (eql ,event-type))
                                   data)
     (funcall ,@(when func `(#',func)) data)))
