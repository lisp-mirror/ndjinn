(in-package #:pyx)

(defvar *game-state* nil)

(defclass game-state ()
  ((%clock :reader clock
           :initarg :clock)
   (%debug-p :reader debug-p
             :initform t)
   (%input-state :reader input-state
                 :initform (make-instance 'input-state))
   (%display :accessor display)
   (%scene-tree :reader scene-tree)
   (%running-p :accessor running-p
               :initform nil)))

(defun iterate-main-loop (game-state)
  (with-continue-restart "Pyx"
    (clock-tick game-state)
    (handle-events game-state)
    (update-game-objects game-state)
    (update-display game-state)
    ;; TODO: Remove this later when possible
    (when (input-enter-p game-state '(:key :escape))
      (stop game-state))))

(defun update-physics (game-state)
  (declare (ignore game-state)))

(defun start ()
  (unwind-protect
       (let ((game-state (make-instance 'game-state)))
         (setup-live-coding)
         (initialize-host game-state)
         (make-clock game-state)
         (make-scene-tree game-state)
         (setf (running-p game-state) t
               *game-state* game-state)
         (log:info :pyx "Started Pyx.")
         (u:while (running-p game-state)
           (iterate-main-loop game-state)))
    (sdl2:sdl-quit)))

(defun stop (game-state)
  (setf (running-p game-state) nil
        *game-state* nil)
  (shutdown-host game-state)
  (log:info :pyx "Stopped Pyx."))
