(in-package #:pyx)

(defvar *game-state* nil)

(defclass game-state ()
  ((%clock :reader clock
           :initarg :clock)
   (%debug-p :reader debug-p
             :initform t)
   (%display :accessor display)
   (%input-state :reader input-state
                 :initform (make-instance 'input-state))
   (%running-p :accessor running-p
               :initform t)
   (%scene-graph :reader scene-graph)
   (%shaders :reader shaders)))

(defmethod initialize-instance :after ((instance game-state) &key)
  (setup-repl)
  (initialize-host instance)
  (initialize-shaders instance)
  (make-clock instance)
  (make-scene-graph instance)
  ;; test
  ;; (let ((test (make-entity instance 'foo)))
  ;;   (add-child test :parent (scene-graph instance)))

  (log:info :pyx "Started Pyx."))

(defun run-main-game-loop (game-state)
  (u:while (running-p game-state)
    (with-continue-restart "Pyx"
      (clock-tick game-state)
      (handle-events game-state)
      (update-display game-state)
      ;; TODO: Remove this later when possible
      (when (input-enter-p game-state '(:key :escape))
        (stop game-state)))))

(defun run-periodic-tasks (game-state)
  (update-repl)
  (recompile-shaders game-state))

(defun start ()
  (unwind-protect
       (let ((*game-state* (make-instance 'game-state)))
         (run-main-game-loop *game-state*))
    (sdl2:sdl-quit)))

(defun stop (game-state)
  (shutdown-host game-state)
  (setf (running-p game-state) nil)
  (log:info :pyx "Stopped Pyx."))
