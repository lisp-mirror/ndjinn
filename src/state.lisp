(in-package #:pyx)

(defclass state ()
  ((%cache :reader cache
           :initform (u:dict #'eq))
   (%camera :reader camera
            :initform nil)
   (%clock :reader clock
           :initarg :clock)
   (%config :reader config)
   (%database :reader database
              :initform (u:dict #'eq))
   (%display :accessor display)
   (%framebuffers :reader framebuffers
                  :initform (u:dict #'eq))
   (%input-state :reader input-state
                 :initform (make-instance 'input-state))
   (%materials :reader materials
               :initform (u:dict #'eq))
   (%node-tree :reader node-tree)
   (%running-p :accessor running-p
               :initform t)
   (%shaders :reader shaders)))

(defmethod initialize-instance :after ((instance state) &rest args
                                       &key &allow-other-keys)
  (let ((*state* instance))
    (setup-repl)
    (apply #'load-config args)
    (make-thread-pool)
    (make-database)
    (prepare-gamepads)
    (make-display)
    (initialize-framebuffers)
    (initialize-shaders)
    (make-node-tree)
    (test)
    (log:info :pyx "Started Pyx.")))

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

(defun start (&rest args)
  (unwind-protect
       (let ((*state* (apply #'make-instance 'state args)))
         (run-main-game-loop))
    (stop)))

(defun stop ()
  (kill-display)
  (destroy-thread-pool)
  (when *state*
    (shutdown-gamepads)
    (setf (running-p *state*) nil)
    (log:info :pyx "Stopped Pyx.")))
