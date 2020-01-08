(in-package #:pyx)

(defclass collider-plan-spec ()
  ((%name :reader name
          :initarg :name)
   (%labels :reader collider-labels
            :initarg :labels
            :initform nil)
   (%plan :reader plan
          :initarg :plan
          :initform (u:dict #'eq))))

(u:define-printer (collider-plan-spec stream)
  (format stream "~s" (name collider-plan-spec)))

(defun update-collider-plan-spec (name data)
  (with-slots (%labels %plan) (meta :collider-plans name)
    (setf %labels (u:plist-keys data))
    (u:do-plist (k v data)
      (setf (u:href %plan k) v))))

(defun make-collider-plan-spec (name data)
  (let ((spec (make-instance 'collider-plan-spec :name name)))
    (setf (meta :collider-plans name) spec)
    (update-collider-plan-spec name data)
    spec))

(defmacro define-collider-plan (name options &body body)
  (declare (ignore options))
  `(progn
     (unless (meta :collider-plans)
       (setf (meta :collider-plans) (u:dict #'eq)))
     (if (meta :collider-plans ',name)
         (update-collider-plan-spec ',name ',(car body))
         (make-collider-plan-spec ',name ',(car body)))))
