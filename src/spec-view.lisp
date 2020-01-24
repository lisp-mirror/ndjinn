(in-package #:pyx)

(defclass view-spec ()
  ((%name :reader name
          :initarg :name)
   (%x :reader x
       :initform 0)
   (%y :reader y
       :initform 0)
   (%width :reader width
           :initform 1)
   (%height :reader height
            :initform 1)))

(u:define-printer (view-spec stream :identity t)
  (format stream "~s" (name view-spec)))

(define-event-handler :recompile :view recompile-viewport)

(defun update-view-spec (name x y width height)
  (let ((view-spec (meta :views name)))
    (with-slots (%x %y %width %height) view-spec
      (setf %x (a:clamp (float x 1f0) 0f0 1f0)
            %y (a:clamp (float y 1f0) 0f0 1f0)
            %width (a:clamp (float width 1f0) 0f0 1f0)
            %height (a:clamp (float height 1f0) 0f0 1f0))
      (enqueue :recompile (list :view)))))

(defun make-view-spec (name x y width height)
  (let ((spec (make-instance 'view-spec :name name)))
    (setf (meta :views name) spec)
    (update-view-spec name x y width height)
    spec))

(defmacro define-view (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key (x 0) (y 0) (width 1) (height 1))
      (car body)
    `(progn
       (unless (meta :views)
         (setf (meta :views) (u:dict #'eq)))
       (if (meta :views ',name)
           (update-view-spec ',name ,x ,y ,width ,height)
           (make-view-spec ',name ,x ,y ,width ,height)))))
