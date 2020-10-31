(in-package #:net.mfiano.lisp.pyx)

(defclass flows ()
  ((%queue :accessor queue
           :initform nil)
   (%buffer :reader buffer
            :initform (make-array 128 :fill-pointer 0 :adjustable t))
   (%priorities :accessor priorities
                :initform nil)))

(defun make-flow-priorities ()
  (let ((table (u:dict #'eq)))
    (setf (u:href table 'transform) 0
          (u:href table 'delete) 1)
    table))

(defun compare-flow-item (item1 item2)
  (let ((priorities (priorities (flows =context=))))
    (< (u:href priorities (car item1))
       (u:href priorities (car item2)))))

(defun make-flows ()
  (let ((flows (make-instance 'flows))
        (queue (queues:make-queue :priority-cqueue
                                  :compare #'compare-flow-item)))
    (setf (queue flows) queue
          (priorities flows) (make-flow-priorities)
          (flows =context=) flows)))

(defun queue-flow-work (type func)
  (let ((flows (flows =context=)))
    (queues:qpush (queue flows) (cons type func))))

(defun process-flows ()
  (let* ((flows (flows =context=))
         (buffer (buffer flows)))
    (loop :with queue = (queue flows)
          :for ((priority . func) found) = (multiple-value-list
                                            (queues:qpop queue))
          :while found
          :do (vector-push-extend func buffer))
    (map nil #'funcall buffer)
    (fill buffer nil)
    (setf (fill-pointer buffer) 0)
    (values)))
