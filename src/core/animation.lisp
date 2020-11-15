(in-package #:ndjinn)

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
                      :initarg :self-finishing
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
  (with-slots (%name %elapsed %duration %self-finishing-p %data %shape)
      animation
    (cond
      ((zerop %elapsed)
       (setf (u:href %data :previous-step) 0f0
             (u:href %data :time) 0f0)
       (on-animate-start entity %name %data))
      ((or (and %self-finishing-p (u:href %data :finished))
           (and (not %self-finishing-p) (>= %elapsed %duration)))
       (on-animate-finish entity %name %data)
       (deregister-animation entity animation)))
    (let ((step (funcall %shape
                         (if (zerop %duration)
                             0f0
                             (u:clamp (/ %elapsed %duration) 0f0 1f0)))))
      (incf %elapsed (get-frame-time))
      (setf (u:href %data :step) step)
      (on-animate-update entity %name %data)
      (setf (u:href %data :previous-step) step
            (u:href %data :time) %elapsed))))

(defun make-animation (entity name
                       &key blocking self-finishing (duration 1)
                         (shape #'math:linear) (where :after) target)
  (let ((animation (make-instance 'animation
                                  :name name
                                  :duration duration
                                  :blocking blocking
                                  :self-finishing self-finishing
                                  :shape shape)))
    (register-animation entity animation :where where :target target)
    animation))

(defgeneric on-animate-start (entity name data)
  (:method (entity name data)))

(defgeneric on-animate-update (entity name data)
  (:method (entity name data)))

(defgeneric on-animate-finish (entity name data)
  (:method (entity name data)))

(defmacro define-animate-hook (hook (entity name data) &body body)
  (let ((method (u:format-symbol :ndjinn "ON-ANIMATE-~a" hook)))
    `(defmethod ,method (,entity (name (eql ',name)) ,data)
       ,@body)))
