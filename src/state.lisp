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
   (%hardware :reader hardware
              :initform nil)
   (%running-p :accessor running-p
               :initform t)))

(defun run-periodic-tasks ()
  #-pyx.release (update-repl)
  (process-queue :recompile))

(defun initialize-engine (scene-name args)
  (cl-renderdoc:mask-overlay-bits :none :none)
  (apply #'load-config args)
  (setup-repl)
  (rng/init)
  (prepare-gamepads)
  (make-display)
  (make-hardware)
  (make-thread-pool)
  (initialize-shaders)
  (switch-scene scene-name)
  (make-clock)
  (log:info :pyx "Started ~a." (cfg :game-title)))

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
           (initialize-engine scene-name args)
           (run-main-game-loop))
      (deinitialize-engine))))

(defun stop-engine ()
  (setf (running-p *state*) nil))
