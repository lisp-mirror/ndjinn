(in-package #:net.mfiano.lisp.pyx)

(defstruct (thread-pool
            (:constructor %make-thread-pool)
            (:predicate nil)
            (:copier nil))
  (worker-count 1 :type fixnum)
  (channels (u:dict #'eq) :type hash-table)
  (queues (u:dict #'eq) :type hash-table))

(defun make-thread-pool ()
  (let* ((worker-count (or (cfg/player :threads)
                           (hardware-info-cpu-count (hardware-info =context=))))
         (thread-pool (%make-thread-pool :worker-count worker-count)))
    (setf lp:*kernel* (lp:make-kernel worker-count)
          (thread-pool =context=) thread-pool)
    (log:debug :pyx "Initialized thread-pool with ~d workers" worker-count)))

(defun destroy-thread-pool ()
  (lp:end-kernel :wait t)
  (when (thread-pool =context=)
    (setf lp:*kernel* nil
          (thread-pool =context=) nil)))

(defun ensure-channel (purpose)
  (when (and =context= (thread-pool =context=))
    (let ((channels (thread-pool-channels (thread-pool =context=))))
      (u:ensure-gethash purpose channels (lp:make-channel)))))

(defun ensure-queue (purpose)
  (when (and =context= (thread-pool =context=))
    (let ((queues (thread-pool-queues (thread-pool =context=))))
      (u:ensure-gethash purpose queues (lpq:make-queue)))))

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
    (let ((queue (ensure-queue purpose)))
      (lpq:push-queue data queue))))

(defun dequeue (purpose)
  (when (and =context= (thread-pool =context=))
    (let ((queue (ensure-queue purpose)))
      (lpq:pop-queue queue))))

(defun queue-empty-p (purpose)
  (let ((queue (ensure-queue purpose)))
    (lpq:queue-empty-p queue)))

(defun process-queue (purpose)
  (u:while (and =context=
                (thread-pool =context=)
                (not (queue-empty-p purpose)))
    (destructuring-bind (&optional event-type data) (dequeue purpose)
      (when event-type
        (handle-queued-event purpose event-type data)))))

(defgeneric handle-queued-event (purpose event-type data)
  (:method (purpose event-type data)
    (error "Unhandled queue event type ~s for queue purpose ~a."
           event-type purpose)))
