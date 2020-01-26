(in-package #:pyx)

(defclass state ()
  ((%clock :reader clock
           :initarg :clock)
   (%config :reader config)
   (%current-scene :reader current-scene
                   :initform nil)
   (%scenes :reader scenes
            :initform (u:dict #'eq))
   (%framebuffers :reader framebuffers
                  :initform (u:dict #'eq))
   (%display :reader display)
   (%input-data :reader input-data
                :initform (make-instance 'input-data))
   (%shaders :reader shaders)
   (%resources :reader resources
               :initform (u:dict #'eq))
   (%running-p :accessor running-p
               :initform t)))

(defun run-periodic-tasks ()
  #-pyx.release (update-repl)
  (process-queue :recompile))

(defun initialize-engine (scene-name)
  (log:info :pyx "Loading ~a..." (cfg :game-title))
  (cl-renderdoc:mask-overlay-bits :none :none)
  (setup-repl)
  (rng/init)
  (make-thread-pool)
  (prepare-gamepads)
  (make-display)
  (initialize-shaders)
  (switch-scene scene-name)
  (make-clock)
  (log:info :pyx "Finished loading ~a." (cfg :game-title)))

(defun deinitialize-engine ()
  (kill-display)
  (destroy-thread-pool)
  (shutdown-gamepads)
  (log:info :pyx "Stopped ~a." (cfg :game-title)))

(defun update-step ()
  (map-nodes #'on-update)
  (perform-input-tasks (input-data *state*))
  (compute-collisions))

(defun run-main-game-loop ()
  (let ((input-data (input-data *state*)))
    (u:while (running-p *state*)
      (with-continue-restart "Pyx"
        (clock-tick #'update-step)
        (process-queue :entity-flow)
        (handle-events input-data)
        (map-nodes #'resolve-model)
        (update-display #'render-frame)))))

(defun start-engine (scene-name &rest args)
  (let ((*state* (make-instance 'state)))
    (unwind-protect
         (progn
           (apply #'load-config args)
           (initialize-engine scene-name)
           (run-main-game-loop))
      (deinitialize-engine))))

(defun stop-engine ()
  (setf (running-p *state*) nil))
