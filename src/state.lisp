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
   (%running-p :accessor running-p
               :initform t)
   (%scene-graph :reader scene-graph)

(defmethod initialize-instance :after ((instance state) &rest args
                                       &key &allow-other-keys)
  (let ((*state* instance))
    (setup-repl)
    (apply #'load-config args)
    (make-thread-pool)
    (make-display)
    (initialize-shaders)
    (make-scene-graph)
    (make-entity (camera)
      :camera/mode :isometric)
    (make-world 1 :width 11 :height 11 :room-extent 3 :seed 1)
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
