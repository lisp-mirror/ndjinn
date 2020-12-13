(in-package #:ndjinn)

(defstruct (collision-plan-spec
            (:constructor %make-collision-plan-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (layers nil :type list)
  (table (u:dict #'eq) :type hash-table))

(u:define-printer (collision-plan-spec stream)
  (format stream "~s" (collision-plan-spec-name collision-plan-spec)))

(defun update-collision-plan-spec (name layers plan)
  (let* ((spec (u:href =meta/collision-plans= name))
         (table (collision-plan-spec-table spec)))
    (setf (collision-plan-spec-layers spec) layers)
    (clrhash table)
    (dolist (x plan)
      (destructuring-bind (k v) x
        (unless (find k layers)
          (error "Collider plan layer ~s is not registered." k))
        (dolist (x v)
          (unless (find x layers)
            (error "Collider plan layer ~s is not registered." x))
          (when (find k (u:href table x))
            (u:deletef v x)))
        (when v
          (setf (u:href table k) v))))))

(defun make-collision-plan-spec (name layers plan)
  (let ((spec (%make-collision-plan-spec :name name)))
    (setf (u:href =meta/collision-plans= name) spec)
    (update-collision-plan-spec name layers plan)
    spec))

(defmacro define-collision-plan (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key layers plan) (car body)
    `(if (u:href =meta/collision-plans= ',name)
         (update-collision-plan-spec ',name ',layers ',plan)
         (make-collision-plan-spec ',name ',layers ',plan))))
