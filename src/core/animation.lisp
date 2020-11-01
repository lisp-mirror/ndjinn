(in-package #:net.mfiano.lisp.pyx)

(defclass animation ()
  ((%name :reader name
          :initarg :name)
   (%elapsed :accessor elapsed
             :initform 0.0)
   (%duration :reader duration
              :initarg :duration)
   (%blocking-p :reader blocking-p
                :initarg :blocking
                :initform nil)
   (%self-finishing-p :reader self-finishing-p
                      :initarg :self-finishing-p
                      :initform nil)
   (%shape :reader shape
           :initarg :shape)
   (%data :reader data
          :initform (u:dict #'equalp))))

(defun register-animation (entity animation &key where target)
  (unless (has-component-p entity 'animate)
    (attach-component entity 'animate))
  (let* ((sequence (animate/sequence entity))
         (target (or target (dll:tail sequence))))
    (dll:insert sequence animation :where where :target target)))

(defun deregister-animation (entity animation)
  (dll:delete (animate/sequence entity) animation))

(defun process-animations (entity)
  (loop :for animation :in (dll:list-values (animate/sequence entity))
        :do (step-animation entity animation)
        :when (blocking-p animation)
          :do (return)))

(defun step-animation (entity animation)
  (let ((name (name animation))
        (duration (duration animation))
        (elapsed (elapsed animation))
        (data (data animation)))
    (cond
      ((zerop elapsed)
       (setf (u:href data :previous-step) 0f0)
       (on-animate-start entity name data))
      ((or (and (self-finishing-p animation)
                (u:href data :finished))
           (>= elapsed duration))
       (on-animate-finish entity name data)
       (deregister-animation entity animation)))
    (incf (elapsed animation) (get-frame-time))
    (let ((step (funcall (shape animation)
                         (u:clamp (/ elapsed duration) 0f0 1f0))))
      (setf (u:href data :step) step)
      (on-animate-update entity name data)
      (setf (u:href data :previous-step) step))))

(defun make-animation (entity name
                       &key blocking (duration 1) (shape #'math:linear)
                         (where :after) target)
  (let ((animation (make-instance 'animation
                                  :name name
                                  :duration duration
                                  :blocking blocking
                                  :shape shape)))
    (register-animation entity animation :where where :target target)
    animation))

(defun finish-animation (animation)
  (setf (u:href (data animation) :finished) t))

(defgeneric on-animate-start (entity name data)
  (:method (entity name data)))

(defgeneric on-animate-update (entity name data)
  (:method (entity name data)))

(defgeneric on-animate-finish (entity name data)
  (:method (entity name data)))

(defmacro define-animate-hook (hook (entity name data) &body body)
  (let ((method (u:format-symbol :net.mfiano.lisp.pyx "ON-ANIMATE-~a" hook)))
    `(defmethod ,method (,entity (name (eql ',name)) ,data)
       ,@body)))
