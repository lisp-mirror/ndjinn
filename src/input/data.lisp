(in-package #:pyx)

(defclass input-data ()
  ((%gamepad-instances :reader gamepad-instances
                       :initform (u:dict #'eq))
   (%gamepad-ids :reader gamepad-ids
                 :initform (u:dict #'eq))
   (%detached-gamepads :accessor detached-gamepads
                       :initform nil)
   (%entering :reader entering
              :initform (u:dict #'eq))
   (%exiting :reader exiting
             :initform (u:dict #'eq))
   (%states :reader states
            :initform (u:dict #'equal))))

(defun make-input-data ()
  (let ((input-data (make-instance 'input-data))
        (motion-state (make-mouse-motion-state)))
    (setf (u:href (states input-data) '(:mouse :motion)) motion-state
          (input-data) input-data)))
