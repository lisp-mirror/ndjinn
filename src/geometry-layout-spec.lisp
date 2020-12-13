(in-package #:ndjinn)

(defstruct (geometry-layout-spec
            (:constructor %make-geometry-layout-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (groups (u:dict #'eq) :type hash-table)
  (group-order nil :type list))

(defun find-geometry-layout (layout-name)
  (or (u:href =meta/geometry-layouts= layout-name)
      (error "Geometry layout ~s not found." layout-name)))

(defun update-geometry-layout-spec (name groups order)
  (let ((layout (u:href =meta/geometry-layouts= name)))
    (setf (geometry-layout-spec-groups layout) groups
          (geometry-layout-spec-group-order layout) order)))

(defun make-geometry-layout-spec (name groups order)
  (let ((layout (%make-geometry-layout-spec :name name)))
    (setf (u:href =meta/geometry-layouts= name) layout)
    (update-geometry-layout-spec name groups order)))

(defmacro define-geometry-layout (name options &body body)
  (declare (ignore options))
  (u:with-gensyms (groups order)
    `(u:mvlet ((,groups ,order (make-geometry-groups ',body)))
       (if (u:href =meta/geometry-layouts= ',name)
           (update-geometry-layout-spec ',name ,groups ,order)
           (make-geometry-layout-spec ',name ,groups ,order)))))
