(in-package #:pyx)

(defclass state ()
  ((%camera :reader camera
            :initform nil)
   (%clock :reader clock
           :initarg :clock)
   (%config :reader config)
   (%database :reader database
              :initform (u:dict #'eq))
   (%display :accessor display)
   (%input-state :reader input-state
                 :initform (make-instance 'input-state))
   (%node-tree :reader node-tree)
   (%resources :reader resources
               :initform (u:dict #'eq))
   (%running-p :accessor running-p
               :initform t)))

(defun initialize-engine (entry-point)
  (log:info :pyx "Loading ~a..." (cfg :game-title))
  (setup-repl)
  (make-thread-pool)
  (make-database)
  (prepare-gamepads)
  (make-display)
  (initialize-framebuffers)
  (initialize-shaders)
  (make-node-tree)
  (load-prefab entry-point)
  (log:info :pyx "Finished loading ~a." (cfg :game-title)))

(defun run-main-game-loop ()
  (make-clock)
  (u:while (running-p *state*)
    (with-continue-restart "Pyx"
      (clock-tick)
      (process-queue :entity-flow)
      (handle-events)
      (update-display)
      ;; TODO: Remove this later when possible
      (when (input-enter-p :key :escape)
        (stop)))))

(defun run-periodic-tasks ()
  (update-repl)
  (process-queue :recompile))

(defun start (entry-point &rest args)
  (unwind-protect
       (let ((*state* (make-instance 'state)))
         (apply #'load-config args)
         (initialize-engine entry-point)
         (run-main-game-loop))
    (stop)))

(defun stop ()
  (kill-display)
  (destroy-thread-pool)
  (when *state*
    (shutdown-gamepads)
    (setf (running-p *state*) nil)
    (log:info :pyx "Stopped ~a." (cfg :game-title))))
