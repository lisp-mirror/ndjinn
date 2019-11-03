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

(defmacro define-geometry-layout (name &body body)
  (a:with-gensyms (groups order)
    `(u:mvlet ((,groups ,order (make-geometry-groups ',body)))
       (unless (meta :geometry-layouts)
         (setf (meta :geometry-layouts) (u:dict)))
       (setf (meta :geometry-layouts ',name)
             (make-instance 'geometry-layout
                            :name ',name
                            :groups ,groups
                            :group-order ,order)))))
