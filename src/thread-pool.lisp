(in-package #:%pyx.thread-pool)

(glob:define-global-var =thread-pool= nil)

(defstruct (thread-pool (:constructor %make-thread-pool)
                        (:conc-name nil)
                        (:predicate nil)
                        (:copier nil))
  worker-count
  (channels (u:dict #'eq))
  (queues (u:dict #'eq)))

(defun make-thread-pool ()
  (let* ((worker-count (or cfg:=threads= hw:=cpu-count=))
         (thread-pool (%make-thread-pool :worker-count worker-count)))
    (setf lparallel:*kernel* (lp:make-kernel worker-count)
          =thread-pool= thread-pool)))

(defun destroy ()
  (lparallel:end-kernel :wait t)
  (when =thread-pool=
    (setf lp:*kernel* nil
          =thread-pool= nil)))

(defun ensure-channel (purpose)
  (let ((channels (channels =thread-pool=)))
    (a:ensure-gethash purpose channels (lp:make-channel))))

(defun ensure-queue (purpose)
  (let ((queues (queues =thread-pool=)))
    (a:ensure-gethash purpose queues (q:make-queue))))

(defun submit-job (purpose job &optional (priority :default))
  (when =thread-pool=
    (let ((channel (ensure-channel purpose))
          (lp:*task-priority* priority)
          (lp:*task-category* purpose))
      (lp:submit-task channel job))))

(defun get-job-results (purpose)
  (when =thread-pool=
    (let ((channel (ensure-channel purpose)))
      (lp:receive-result channel))))

(defun kill-jobs (purpose)
  (lp:kill-tasks purpose))

(defun enqueue (purpose data)
  (when =thread-pool=
    (let ((queue (ensure-queue purpose)))
      (q:push-queue data queue))))

(defun dequeue (purpose)
  (when =thread-pool=
    (let ((queue (ensure-queue purpose)))
      (q:pop-queue queue))))

(defun queue-empty-p (purpose)
  (let ((queue (ensure-queue purpose)))
    (q:queue-empty-p queue)))

(defun process-queue (purpose)
  (u:while (and =thread-pool= (not (queue-empty-p purpose)))
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
