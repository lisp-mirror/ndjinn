(in-package #:%pyx.collision-detection)

(defstruct (plan-spec (:constructor %make-plan-spec)
                      (:conc-name nil)
                      (:predicate nil)
                      (:copier nil))
  name
  layers
  (plan (u:dict #'eq)))

(u:define-printer (plan-spec stream)
  (format stream "~s" (name plan-spec)))

(defun update-plan-spec (name layers plan)
  (let ((spec (u:href meta:=collider-plans= name)))
    (setf (layers spec) layers)
    (clrhash (plan spec))
    (dolist (x plan)
      (destructuring-bind (k v) x
        (unless (find k layers)
          (error "Collider plan layer ~s is not registered." k))
        (dolist (x v)
          (unless (find x layers)
            (error "Collider plan layer ~s is not registered." x))
          (when (find k (u:href (plan spec) x))
            (a:deletef v x)))
        (when v
          (setf (u:href (plan spec) k) v))))))

(defun make-plan-spec (name layers plan)
  (let ((spec (%make-plan-spec :name name)))
    (setf (u:href meta:=collider-plans= name) spec)
    (update-plan-spec name layers plan)
    spec))

;;; Public API

(defmacro define-collider-plan (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key layers plan) (car body)
    `(if (u:href meta:=collider-plans= ',name)
         (update-plan-spec ',name ',layers ',plan)
         (make-plan-spec ',name ',layers ',plan))))

(define-collider-plan :default ())
