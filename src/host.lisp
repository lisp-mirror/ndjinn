(in-package #:pyx)

;;; TODO: Add gamepad init/shutdown

(defun initialize-host (game-state)
  (let ((flags '(:everything)))
    (unless (apply #'sdl2:was-init flags)
      (let ((flags (autowrap:mask-apply 'sdl2::sdl-init-flags flags)))
        (sdl2::check-rc (sdl2::sdl-init flags))))
    (setf (display game-state) (make-display game-state))))

(defun shutdown-host (game-state)
  (sdl2:destroy-window (window (display game-state)))
  (sdl2::sdl-quit))
