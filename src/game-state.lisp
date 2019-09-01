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
   (%running-p :accessor running-p
               :initform nil)))

(defun make-game-state ()
  (make-instance 'game-state
                 :clock (make-clock)))

(defun iterate-main-loop (game-state)
  (with-continue-restart "Pyx"
    (handle-events game-state)
    (render-screen game-state)
    (clock-tick (clock game-state) (refresh-rate (display game-state)))
    ;; TODO: Remove this later when possible
    (when (input-enter-p game-state '(:key :escape))
      (stop game-state))))

(defun start ()
  (unwind-protect
       (let ((game-state (make-game-state)))
         ;; TODO: init
         (setup-live-coding)
         (initialize-host game-state)
         (log:info :pyx "Started Pyx.")
         (setf (running-p game-state) t
               *game-state* game-state)
         (initialize-clock game-state)
         (u:while (running-p game-state)
           (iterate-main-loop game-state)))
    (sdl2:sdl-quit)))

(defun stop (game-state)
  (setf (running-p game-state) nil
        *game-state* nil)
  (shutdown-host game-state)
  (log:info :pyx "Shut down Pyx."))
