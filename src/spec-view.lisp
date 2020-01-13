(in-package #:pyx)

(defclass view-spec ()
  ((%name :reader name
          :initarg :name)
   (%x :reader x
       :initform 0f0)
   (%y :reader y
       :initform 0f0)
   (%width :reader width
           :initform 1f0)
   (%height :reader height
            :initform 1f0)))

(defun update-view-spec (name x y width height)
  (with-slots (%x %y %width %height) (meta :views name)
    (setf %x (a:clamp (float x 1f0) 0f0 1f0)
          %y (a:clamp (float y 1f0) 0f0 1f0)
          %width (a:clamp (float width 1f0) 0f0 1f0)
          %height (a:clamp (float height 1f0) 0f0 1f0))))

(defun make-view-spec (name x y width height)
  (let ((spec (make-instance 'view-spec :name name)))
    (setf (meta :views name) spec)
    (update-view-spec name x y width height)
    spec))

(defmacro define-view (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key (x 0f0) (y 0f0) (width 1f0) (height 1f0)) (car body)
    `(progn
       (unless (meta :views)
         (setf (meta :views) (u:dict #'eq)))
       (if (meta :views ',name)
           (update-view-spec ',name ,x ,y ,width ,height)
           (make-view-spec ',name ,x ,y ,width ,height)))))
