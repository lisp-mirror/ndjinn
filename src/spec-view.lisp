(in-package #:pyx)

(defclass view-spec ()
  ((%name :reader name
          :initarg :name)
   (%tags :reader tags
          :initarg :tags)
   (%x :reader x
       :initform 0f0)
   (%y :reader y
       :initform 0f0)
   (%width :reader width
           :initform 1f0)
   (%height :reader height
            :initform 1f0)))

(u:define-printer (view-spec stream :identity t)
  (format stream "~s" (name view-spec)))

(defun update-view-tags (view-spec tags)
  (with-slots (%name %tags) view-spec
    (dolist (tag %tags)
      (a:deletef (meta :view-tags tag) %name)
      (unless (meta :view-tags tag)
        (remhash tag (meta :view-tags))))
    (dolist (tag tags)
      (pushnew %name (meta :view-tags tag)))))

(defun update-view-spec (name tags x y width height)
  (let ((view-spec (meta :views name)))
    (with-slots (%x %tags %y %width %height) view-spec
      (update-view-tags view-spec tags)
      (setf %tags tags
            %x (a:clamp (float x 1f0) 0f0 1f0)
            %y (a:clamp (float y 1f0) 0f0 1f0)
            %width (a:clamp (float width 1f0) 0f0 1f0)
            %height (a:clamp (float height 1f0) 0f0 1f0)))))

(defun make-view-spec (name tags x y width height)
  (let ((spec (make-instance 'view-spec :name name)))
    (setf (meta :views name) spec)
    (update-view-spec name tags x y width height)
    spec))

(defmacro define-view (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key tags (x 0f0) (y 0f0) (width 1f0) (height 1f0))
      (car body)
    `(progn
       (unless (meta :views)
         (setf (meta :views) (u:dict #'eq)))
       (unless (meta :view-tags)
         (setf (meta :view-tags) (u:dict #'eq)))
       (if (meta :views ',name)
           (update-view-spec ',name ',tags ,x ,y ,width ,height)
           (make-view-spec ',name ',tags ,x ,y ,width ,height)))))
