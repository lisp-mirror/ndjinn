(in-package #:ndjinn)

(defstruct (animation
            (:constructor %make-animation)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (elapsed 0f0 :type single-float)
  (duration 1f0 :type single-float)
  (blocking-p nil :type boolean)
  (self-finishing-p nil :type boolean)
  (shape #'math:linear :type function)
  (data (u:dict #'equalp) :type hash-table))

(defun register-animation (entity animation &key where target)
  (unless (has-component-p entity 'animate)
    (attach-component entity 'animate))
  (let* ((sequence (animate/sequence entity))
         (target (or target (dll:tail sequence))))
    (dll:insert sequence animation :where where :target target)))

(defun deregister-animation (entity animation)
  (dll:delete (animate/sequence entity) animation))

(defgeneric on-animate-start (entity name data)
  (:method (entity name data)))

(defgeneric on-animate-update (entity name data)
  (:method (entity name data)))

(defgeneric on-animate-finish (entity name data)
  (:method (entity name data)))

(defun step-animation (entity animation)
  (let ((name (animation-name animation))
        (duration (animation-duration animation))
        (elapsed (animation-elapsed animation))
        (self-finishing-p (animation-self-finishing-p animation))
        (data (animation-data animation)))
    (cond
      ((zerop elapsed)
       (setf (u:href data :previous-step) 0f0
             (u:href data :time) 0f0)
       (on-animate-start entity name data))
      ((or (and self-finishing-p (u:href data :finished))
           (and (not self-finishing-p)
                (>= elapsed duration)))
       (on-animate-finish entity name data)
       (deregister-animation entity animation)))
    (let ((step (funcall (animation-shape animation)
                         (if (zerop duration)
                             0f0
                             (u:clamp (/ elapsed duration) 0f0 1f0)))))
      (incf (animation-elapsed animation) (get-frame-time))
      (setf (u:href data :step) step)
      (on-animate-update entity name data)
      (setf (u:href data :previous-step) step
            (u:href data :time) (animation-elapsed animation)))))

(defun process-animations (entity)
  (loop :for animation :in (dll:list-values (animate/sequence entity))
        :do (step-animation entity animation)
        :when (animation-blocking-p animation)
          :do (return)))

(defun make-animation (entity name
                       &key blocking self-finishing (duration 1f0)
                         (shape #'math:linear) (where :after) target)
  (let ((animation (%make-animation :name name
                                    :duration (float duration 1f0)
                                    :blocking-p blocking
                                    :self-finishing-p self-finishing
                                    :shape shape)))
    (register-animation entity animation :where where :target target)
    animation))

(defmacro define-animate-hook (hook (entity name data) &body body)
  (let ((method (u:format-symbol :ndjinn "ON-ANIMATE-~a" hook)))
    `(defmethod ,method (,entity (name (eql ',name)) ,data)
       ,@body)))
