(in-package #:%pyx.geometry)

(defstruct (layout (:constructor %make-layout)
                   (:conc-name nil)
                   (:predicate nil)
                   (:copier nil))
  layout-name
  groups
  group-order)

(defun find-layout (layout-name)
  (or (u:href meta:=geometry-layouts= layout-name)
      (error "Geometry layout ~s not found." layout-name)))

(defun make-layout (name groups order)
  (let ((layout (%make-layout :layout-name name)))
    (setf (u:href meta:=geometry-layouts= name) layout)
    (update-layout name groups order)))

(defun update-layout (name groups order)
  (let ((layout (u:href meta:=geometry-layouts= name)))
    (setf (groups layout) groups
          (group-order layout) order)))

(defmacro define-geometry-layout (name options &body body)
  (declare (ignore options))
  (a:with-gensyms (groups order)
    `(u:mvlet ((,groups ,order (make-groups ',body)))
       (if (u:href meta:=geometry-layouts= ',name)
           (update-layout ',name ,groups ,order)
           (make-layout ',name ,groups ,order)))))
