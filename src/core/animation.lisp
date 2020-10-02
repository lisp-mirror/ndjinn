(in-package #:net.mfiano.lisp.pyx)

(defclass animation ()
  ((%name :reader name
          :initarg :name)
   (%elapsed :accessor elapsed
             :initform 0)
   (%duration :reader duration
              :initarg :duration)
   (%blocking-p :reader blocking-p
                :initarg :blocking
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
  (let ((sequence (animate/sequence entity)))
    (dll:delete sequence animation)))

(defun process-animations (entity)
  (loop :with sequence = (animate/sequence entity)
        :for animation :in (dll:list-values sequence)
        :do (step-animation entity animation)
        :when (blocking-p animation)
          :do (return)))

(defun step-animation (entity animation)
  (with-slots (%name %elapsed %duration %shape %data) animation
    (cond
      ((zerop %elapsed)
       (on-animate-start entity %name %data))
      ((>= %elapsed %duration)
       (on-animate-finish entity %name %data)
       (deregister-animation entity animation)))
    (incf %elapsed (get-frame-time))
    (let ((delta (funcall %shape (u:clamp (/ %elapsed %duration) 0f0 1f0))))
      (setf (u:href %data :delta) delta)
      (on-animate-update entity %name %data))))

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
