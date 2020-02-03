(in-package #:%pyx.core)

(defun initialize (scene-name args)
  (apply #'cfg:load args)
  (util:initialize-rng)
  (in:prepare-gamepads)
  (display:make-display)
  (in:make-input-data)
  (hw:load)
  (tp:make-thread-pool)
  (shader:initialize)
  (scene:switch-scene scene-name)
  (clock:make-clock)
  (live:setup-repl)
  (start-loop))

(defun deinitialize ()
  (display:kill)
  (tp:destroy)
  (in:shutdown-gamepads))

(defun update ()
  (let ((alpha (clock:get-alpha)))
    (c/node:do-nodes (node)
      (c/transform:resolve-model node alpha)
      (ent:on-update node))))

(defun physics-update ()
  (c/node:map-nodes #'ent:on-physics-update)
  (c/node:do-nodes (node)
    (c/transform:transform-node node))
  (cd:compute-collisions))

(defun periodic-update ()
  #-pyx.release (live:update-repl)
  (tp:process-queue :recompile))

(defun start-loop ()
  (let* ((clock (ctx:clock))
         (display (ctx:display))
         (input-data (ctx:input-data))
         (refresh-rate (display:refresh-rate display)))
    (update)
    (u:while (ctx:running-p)
      (live:with-continuable "Pyx"
        (in:handle-events input-data)
        (clock:tick clock refresh-rate #'physics-update #'periodic-update)
        (update)
        (display:render display)))))

;;; Public API

(defun start-engine (scene-name &rest args)
  (let ((ctx:*context* (ctx:make-context)))
    (unwind-protect (initialize scene-name args)
      (deinitialize))))

(defun stop-engine ()
  (setf (ctx:running-p) nil))