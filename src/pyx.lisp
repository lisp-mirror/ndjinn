(in-package #:net.mfiano.lisp.pyx)

(defun initialize (user-args)
  (start-logging)
  (log:info :pyx.core "Starting ~a..." (cfg :title))
  (load-player-config)
  (setup-repl)
  (initialize-rng)
  (prepare-gamepads)
  (make-display)
  (make-input-data)
  (load-hardware-info)
  (make-thread-pool)
  (initialize-shaders)
  (make-clock)
  (apply #'on-context-create =context= user-args)
  (log:info :pyx.core "Started ~a" (cfg :title))
  (start-loop))

(defun deinitialize ()
  (unwind-protect
       (progn
         (log:info :pyx.core "Shutting down ~a..." (cfg :title))
         (on-context-destroy =context=)
         (shutdown-gamepads)
         (kill-display)
         (destroy-thread-pool)
         (sdl2:quit)
         (stop-logging)
         (log:info :pyx.core "Exited ~a" (cfg :title)))
    (setf =context= nil)))

(defun process-end-frame-work ()
  (lp:pmap nil #'funcall (nreverse (end-frame-work =context=)))
  (setf (end-frame-work =context=) nil))

(defun update ()
  (let ((alpha (get-alpha)))
    (do-nodes (node)
      (on-update node))
    (process-end-frame-work)
    (do-nodes (node)
      (resolve-model node alpha))))

(defun physics-update ()
  (do-nodes (node)
    (on-physics-update node)
    (transform-node node))
  (compute-collisions))

(defun periodic-update ()
  (update-repl)
  (process-queue :recompile))

(defun start-loop ()
  (let* ((clock (clock =context=))
         (display (display =context=))
         (input-data (input-data =context=))
         (refresh-rate (display-refresh-rate display)))
    (update)
    (log:debug :pyx.core "Entered main game loop")
    (u:while (running =context=)
      (with-continuable "Pyx"
        (handle-events input-data)
        (tick-clock clock refresh-rate #'physics-update #'periodic-update)
        (update)
        (render display)))))

(defun start-engine (context-name &rest user-args)
  (unless (and =context= (running =context=))
    (setf =context= (make-context context-name))
    (unwind-protect (initialize user-args)
      (deinitialize))))

(defun stop-engine ()
  (setf (running =context=) nil))

(defun pause-game ()
  (setf (paused (current-scene =context=)) t))

(defun unpause-game ()
  (setf (paused (current-scene =context=)) nil))

(defun toggle-pause ()
  (let ((scene (current-scene =context=)))
    (setf (paused scene) (not (paused scene)))))
