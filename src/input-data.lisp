(in-package #:%pyx.input)

(defstruct (input-data (:constructor %make-input-data)
                       (:conc-name nil)
                       (:predicate nil)
                       (:copier nil))
  (gamepad-instances (u:dict #'eq))
  (gamepad-ids (u:dict #'eq))
  detached-gamepads
  (entering (u:dict #'eq))
  (exiting (u:dict #'eq))
  (states (u:dict #'equal '(:mouse :motion) (make-mouse-motion-state))))

(defun make-input-data ()
  (setf (ctx:input-data) (%make-input-data)))
