(in-package #:net.mfiano.lisp.pyx)

(defstruct (geometry-layout
            (:constructor %make-geometry-layout)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (groups (u:dict #'eq) :type hash-table)
  (group-order nil :type list))

(defun find-geometry-layout (layout-name)
  (or (u:href (metadata-geometry-layouts =metadata=) layout-name)
      (error "Geometry layout ~s not found." layout-name)))

(defun make-geometry-layout (name groups order)
  (let ((layout (%make-geometry-layout :name name)))
    (setf (u:href (metadata-geometry-layouts =metadata=) name) layout)
    (update-geometry-layout name groups order)))

(defun update-geometry-layout (name groups order)
  (let ((layout (u:href (metadata-geometry-layouts =metadata=) name)))
    (setf (geometry-layout-groups layout) groups
          (geometry-layout-group-order layout) order)))

(defmacro define-geometry-layout (name options &body body)
  (declare (ignore options))
  (u:with-gensyms (groups order)
    `(u:mvlet ((,groups ,order (make-geometry-groups ',body)))
       (if (u:href (metadata-geometry-layouts =metadata=) ',name)
           (update-geometry-layout ',name ,groups ,order)
           (make-geometry-layout ',name ,groups ,order)))))
