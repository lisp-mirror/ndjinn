(in-package #:net.mfiano.lisp.pyx)

(defstruct (input-data
            (:constructor %make-input-data)
            (:predicate nil)
            (:copier nil))
  (gamepad-instances (u:dict #'eq) :type hash-table)
  (gamepad-ids (u:dict #'eq) :type hash-table)
  (detached-gamepads nil :type list)
  (entering (u:dict #'eq) :type hash-table)
  (exiting (u:dict #'eq) :type hash-table)
  (states (u:dict #'equal) :type hash-table))

(defun make-input-data ()
  (let ((data (%make-input-data))
        (motion-state (make-mouse-motion-state)))
    (setf (u:href (input-data-states data) '(:mouse :motion)) motion-state
          (input-data =context=) data)))
