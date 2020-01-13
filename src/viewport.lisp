(in-package #:pyx)

(defclass viewport ()
  ((%spec :reader spec
          :initarg :spec)
   (%x :reader x
       :initform 0f0)
   (%y :reader y
       :initform 0f0)
   (%width :reader width
           :initform 0f0)
   (%height :reader height
            :initform 0f0)))

(defun make-viewport (view-spec)
  (let* ((spec (meta :views view-spec))
         (viewport (make-instance 'viewport :spec spec)))
    (configure-viewport viewport)
    viewport))

(defun configure-viewport (viewport)
  (with-slots (%spec %x %y %width %height) viewport
    (let ((window-width (cfg :window-width))
          (window-height (cfg :window-height)))
      (setf %x (u:map-domain 0 1 0 window-width (x %spec))
            %y (u:map-domain 0 1 0 window-height (y %spec))
            %width (u:map-domain 0 1 0 window-width (width %spec))
            %height (u:map-domain 0 1 0 window-height (height %spec)))
      (gl:viewport %x %y %width %height))))

(defun get-viewport-dimensions ()
  (let ((camera (camera (current-scene *state*))))
    (with-slots (%width %height) (camera/viewport camera)
      (v2:vec %width %height))))
