(in-package #:pyx)

(defclass collider-plan-spec ()
  ((%name :reader name
          :initarg :name)
   (%layers :reader layers
            :initarg :layers
            :initform nil)
   (%plan :reader plan
          :initarg :plan
          :initform (u:dict #'eq))))

(u:define-printer (collider-plan-spec stream)
  (format stream "~s" (name collider-plan-spec)))

(defun update-collider-plan-spec (name layers plan)
  (with-slots (%layers %plan) (meta :collider-plans name)
    (setf %layers layers)
    (dolist (x plan)
      (destructuring-bind (k v) x
        (unless (find k layers)
          (error "Collider plan layer ~s is not registered." k))
        (dolist (x v)
          (unless (find x layers)
            (error "Collider plan layer ~s is not registered." x))
          (when (find k (u:href %plan x))
            (a:deletef v x)))
        (when v
          (setf (u:href %plan k) v))))))

(defun make-collider-plan-spec (name layers plan)
  (let ((spec (make-instance 'collider-plan-spec :name name)))
    (setf (meta :collider-plans name) spec)
    (update-collider-plan-spec name layers plan)
    spec))

(defmacro define-collider-plan (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key layers plan) (car body)
    `(progn
       (unless (meta :collider-plans)
         (setf (meta :collider-plans) (u:dict #'eq)))
       (if (meta :collider-plans ',name)
           (update-collider-plan-spec ',name ',layers ',plan)
           (make-collider-plan-spec ',name ',layers ',plan)))))
