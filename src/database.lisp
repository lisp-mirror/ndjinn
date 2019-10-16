(in-package #:pyx)

(defclass database ()
  ((%uuids :reader uuids
           :initform (u:dict #'eq))
   (%released-picking-ids :accessor released-picking-ids
                          :initform nil)
   (%picking-ids :reader picking-id
                 :initform (u:dict #'eq))))

(defun make-database ()
  (setf (slot-value *state* '%database) (make-instance 'database)))

(defgeneric find-by-uuid (uuid)
  (:method ((uuid uuid))
    (u:href (uuids (database *state*)) uuid))
  (:method ((uuid string))
    (u:href (uuids (database *state*)) (string->uuid uuid))))

(defun find-by-picking-id (id)
  (u:href (picking-id (database *state*)) id))

(defun generate-picking-id ()
  (with-slots (%database) *state*
    (let* ((table (picking-id %database))
           (id-count (hash-table-count table)))
      (if (zerop id-count)
          0
          (or (pop (released-picking-ids %database))
              id-count)))))
