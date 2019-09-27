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
    (setf lparallel:*kernel* (lparallel:make-kernel
                              worker-count
                              ;; :bindings '((*state* . *state*))
                              )
          *thread-pool* thread-pool)))

(defun destroy-thread-pool ()
  (lparallel:end-kernel)
  (setf *thread-pool* nil))

(defun ensure-channel (purpose)
  (let ((channels (channels *thread-pool*)))
    (a:ensure-gethash purpose channels (lparallel:make-channel))))

(defun ensure-queue (purpose)
  (let ((queues (queues *thread-pool*)))
    (a:ensure-gethash purpose queues (lparallel.queue:make-queue))))

(defun submit-job (purpose job &optional (priority :high))
  (let ((channel (ensure-channel purpose))
        (lparallel:*task-priority* priority))
    (lparallel:submit-task channel job)))

(defun get-job-results (purpose)
  (let ((channel (ensure-channel purpose)))
    (lparallel:receive-result channel)))

(defun push-queue (purpose data)
  (let ((queue (ensure-queue purpose)))
    (lparallel.queue:push-queue data queue)))

(defun pop-queue (purpose)
  (let ((queue (ensure-queue purpose)))
    (unless (lparallel.queue:queue-empty-p queue)
      (let ((result (lparallel.queue:pop-queue queue)))
        (values result t)))))
