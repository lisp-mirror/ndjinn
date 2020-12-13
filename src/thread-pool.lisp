(in-package #:ndjinn)

(defstruct (thread-pool
            (:constructor %make-thread-pool)
            (:predicate nil)
            (:copier nil))
  (worker-count 1 :type (integer 1 #.most-positive-fixnum))
  (channels (u:dict #'eq) :type hash-table)
  (queues (u:dict #'eq) :type hash-table))

(defun make-thread-pool ()
  (let* ((worker-count (or (cfg/player :threads)
                           (hardware-info-cpu-count (hardware-info =context=))))
         (thread-pool (%make-thread-pool :worker-count worker-count)))
    (setf lp:*kernel* (lp:make-kernel worker-count)
          (thread-pool =context=) thread-pool)
    (log:debug :ndjinn "Initialized thread-pool with ~d workers" worker-count)))

(defun destroy-thread-pool ()
  (u:when-let ((thread-pool (thread-pool =context=)))
    (u:do-hash-keys (purpose (thread-pool-channels thread-pool))
      (lp:kill-tasks purpose))
    (lp:end-kernel :wait t)
    (setf lp:*kernel* nil
          (thread-pool =context=) nil)))

(defun ensure-channel (purpose)
  (u:when-let* ((thread-pool (and =context= (thread-pool =context=)))
                (channels (thread-pool-channels thread-pool)))
    (u:ensure-gethash purpose channels (lp:make-channel))))

(defun ensure-queue (purpose)
  (u:when-let* ((thread-pool (and =context= (thread-pool =context=)))
                (queues (thread-pool-queues thread-pool)))
    (u:ensure-gethash purpose queues (lpq:make-queue))))

(defun submit-job (purpose job &optional (priority :default))
  (when (and =context= (thread-pool =context=))
    (let ((channel (ensure-channel purpose))
          (lp:*task-priority* priority)
          (lp:*task-category* purpose))
      (lp:submit-task channel job))))

(defun get-job-results (purpose)
  (when (and =context= (thread-pool =context=))
    (let ((channel (ensure-channel purpose)))
      (lp:receive-result channel))))

(defun kill-jobs (purpose)
  (lp:kill-tasks purpose))

(defun enqueue (purpose data)
  (when (and =context= (thread-pool =context=))
    (lpq:push-queue data (ensure-queue purpose))))

(defun dequeue (purpose)
  (when (and =context= (thread-pool =context=))
    (lpq:pop-queue (ensure-queue purpose))))

(defun queue-empty-p (purpose)
  (lpq:queue-empty-p (ensure-queue purpose)))

(defgeneric handle-queued-event (purpose event-type data)
  (:method (purpose event-type data)
    (error "Unhandled queue event type ~s for queue purpose ~a."
           event-type purpose)))

(defun process-queue (purpose)
  (u:while (and =context=
                (thread-pool =context=)
                (not (queue-empty-p purpose)))
    (destructuring-bind (&optional event-type data) (dequeue purpose)
      (when event-type
        (handle-queued-event purpose event-type data)))))
