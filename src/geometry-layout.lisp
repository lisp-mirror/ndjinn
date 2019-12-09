(in-package #:pyx)

(defclass geometry-layout ()
  ((%name :reader name
          :initarg :name)
   (%groups :reader groups
            :initarg :groups
            :initform nil)
   (%group-order :reader group-order
                 :initarg :group-order)))

(defun find-geometry-layout (layout-name)
  (or (u:href (meta :geometry-layouts) layout-name)
      (error "Geometry layout ~s not found." layout-name)))

(defun make-geometry-layout (name groups order)
  (let ((layout (make-instance 'geometry-layout :name name)))
    (setf (meta :geometry-layouts name) layout)
    (update-geometry-layout name groups order)))

(defun update-geometry-layout (name groups order)
  (with-slots (%groups %group-order) (meta :geometry-layouts name)
    (setf %groups groups
          %group-order order)))

(defmacro define-geometry-layout (name &body body)
  (a:with-gensyms (groups order)
    `(u:mvlet ((,groups ,order (make-geometry-groups ',body)))
       (unless (meta :geometry-layouts)
         (setf (meta :geometry-layouts) (u:dict #'eq)))
       (if (meta :geometry-layouts ',name)
           (update-geometry-layout ',name ,groups ,order)
           (make-geometry-layout ',name ,groups ,order)))))
