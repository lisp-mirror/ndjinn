(in-package #:pyx)

(defclass database ()
  ((%uuid :reader uuid
          :initform (u:dict #'eq))))

(defun make-database ()
  (setf (slot-value *state* '%database) (make-instance 'database)))

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid))
    (let ((uuids (uuid (database *state*))))
      (u:href uuids uuid)))
  (:method ((uuid string))
    (let ((uuids (uuid (database *state*))))
      (u:href uuids (string->uuid uuid)))))
