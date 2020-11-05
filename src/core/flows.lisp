(in-package #:net.mfiano.lisp.pyx)

(defclass flows ()
  ((%queue :accessor queue
           :initform nil)
   (%priorities :accessor priorities
                :initform nil)))

(defun make-flow-priorities ()
  (let ((table (u:dict #'eq)))
    (setf (u:href table 'transform) 0
          (u:href table 'detach) 1
          (u:href table 'delete) 2)
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
  (loop :with queue = (queue (flows =context=))
        :for ((nil . func) found) = (multiple-value-list (queues:qpop queue))
        :while found
        :do (funcall func)))