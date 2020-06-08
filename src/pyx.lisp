(in-package #:net.mfiano.lisp.pyx)

(defun initialize ()
  (setup-repl)
  (load-config)
  (initialize-rng)
  (prepare-gamepads)
  (make-display)
  (make-input-data)
  (load-hardware-info)
  (make-thread-pool)
  (initialize-shaders)
  (make-clock)
  (on-context-create *context*)
  (start-loop))

(defun deinitialize ()
  (on-context-destroy *context*)
  (kill-display)
  (destroy-thread-pool)
  (shutdown-gamepads)
  (sdl2:quit))

(defun update ()
  (let ((alpha (get-alpha)))
    (do-nodes (node)
      (on-update node))
    (do-nodes (node)
      (resolve-model node alpha))))

(defun physics-update ()
  (map-nodes #'on-physics-update)
  (do-nodes (node)
    (transform-node node))
  (compute-collisions))

(defun periodic-update ()
  #-pyx.release (update-repl)
  (process-queue :recompile))

(defun start-loop ()
  (let* ((clock (clock))
         (display (display))
         (input-data (input-data))
         (refresh-rate (refresh-rate display)))
    (update)
    (u:while (running-p)
      (with-continuable "Pyx"
        (handle-events input-data)
        (tick-clock clock refresh-rate #'physics-update #'periodic-update)
        (update)
        (render display)))))

(defun start-engine (context-name)
  (let ((*context* (make-context context-name)))
    (unwind-protect (initialize)
      (deinitialize))))

(defun stop-engine ()
  (setf (running-p) nil))
