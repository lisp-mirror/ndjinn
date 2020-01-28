(in-package #:pyx)

(defclass viewport-manager ()
  ((%table :reader table
           :initform (u:dict #'eq))
   (%active :accessor active
            :initform nil)
   (%default :accessor default
             :initform nil)
   (%tags :reader tags
          :initform (u:dict #'eq))))

(defclass viewport ()
  ((%spec :reader spec
          :initarg :spec)
   (%camera :accessor camera
            :initform nil)
   (%draw-order :accessor draw-order
                :initform (make-draw-order-tree))
   (%picking-ray :reader picking-ray
                 :initform (make-instance 'picking-ray))
   (%x :reader x
       :initform 0)
   (%y :reader y
       :initform 0)
   (%width :reader width
           :initform 0)
   (%height :reader height
            :initform 0)))

(u:define-printer (viewport stream :identity t)
  (format stream "~s" (name (spec viewport))))

(defun make-viewport (view-spec)
  (let* ((spec (meta :views view-spec))
         (viewport (make-instance 'viewport :spec spec)))
    (configure-viewport viewport)
    viewport))

(defun get-viewport ()
  (active (viewports (get-scene))))

(defun get-entity-viewports (entity)
  (let ((scene (get-scene))
        (viewports nil))
    (dolist (id (id/views entity))
      (let ((viewport (u:href (table (viewports scene)) id)))
        (pushnew viewport viewports)))
    (or viewports (list (default (viewports scene))))))

(defun configure-viewport (viewport)
  (with-slots (%spec %x %y %width %height) viewport
    (setf %x (u:map-domain 0 1 0 cfg:=WINDOW-WIDTH= (x %spec))
          %y (u:map-domain 0 1 0 cfg:=WINDOW-HEIGHT= (y %spec))
          %width (u:map-domain 0 1 0 cfg:=WINDOW-WIDTH= (width %spec))
          %height (u:map-domain 0 1 0 cfg:=WINDOW-HEIGHT= (height %spec)))
    (gl:viewport %x %y %width %height)))

(defun get-viewport-dimensions ()
  (let ((viewport (or (get-viewport)
                      (default (viewports (get-scene))))))
    (with-slots (%width %height) viewport
      (v2:vec %width %height))))

(defun recompile-viewport (name)
  (declare (ignore name))
  (recompile-scene (name (spec (get-scene)))))
