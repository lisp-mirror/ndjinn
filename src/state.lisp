(in-package #:pyx)

(defclass state ()
  ((%cache :reader cache
           :initform (u:dict #'eq))
   (%camera :reader camera)
   (%clock :reader clock
           :initarg :clock)
   (%config :reader config)
   (%display :accessor display)
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
    (make-display)
    (initialize-shaders)
    (make-node-tree)
    (test)
    (log:info :pyx "Started Pyx.")))

(defun run-main-game-loop ()
  (make-clock)
  (u:while (running-p *state*)
    (with-continue-restart "Pyx"
      (clock-tick)
      (handle-events)
      (update-display)
      ;; TODO: Remove this later when possible
      (when (input-enter-p :key :escape)
        (stop)))))

(defun run-periodic-tasks ()
  (update-repl)
  (recompile-jobs))

(defun start (&rest args)
  (unwind-protect
       (let ((*state* (apply #'make-instance 'state args)))
         (run-main-game-loop))
    (stop)))

(defun stop ()
  (kill-display)
  (destroy-thread-pool)
  (when *state*
    (setf (running-p *state*) nil)
    (log:info :pyx "Stopped Pyx.")))
