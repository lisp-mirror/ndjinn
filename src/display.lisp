(in-package #:pyx)

(defclass display ()
  ((%window :accessor window)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)))

(defun make-opengl-context (display)
  (with-slots (%window) display
    (sdl2:gl-set-attrs :context-major-version 4
                       :context-minor-version 5
                       :context-profile-mask 1
                       :multisamplebuffers 1
                       :multisamplesamples 4)
    (sdl2:gl-create-context %window)
    (apply #'gl:enable +gl-capabilities/enabled+)
    (apply #'gl:disable +gl-capabilities/disabled+)
    (apply #'gl:blend-func +gl-blend-mode+)
    (gl:depth-func +gl-depth-mode+)
    (u:noop)))

(defun make-window (display)
  (with-slots (%window) display
    (setf %window (sdl2:create-window :title "Pyx"
                                      :w (cfg :window-width)
                                      :h (cfg :window-height)
                                      :flags '(:opengl)))))

(defun make-display ()
  (sdl2:init :everything)
  (let* ((refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))
         (display (make-instance 'display :refresh-rate refresh-rate)))
    (make-window display)
    (make-opengl-context display)
    (sdl2:gl-set-swap-interval (if (cfg :vsync) 1 0))
    (setf (slot-value *state* '%display) display)
    display))

(defun kill-display ()
  (when (and *state* (display *state*))
    (sdl2:destroy-window (window (display *state*)))
    (setf (slot-value *state* '%display) nil))
  (sdl2:sdl-quit))

(defun clear-screen ()
  (with-slots (%clock) *state*
    (multiple-value-call #'gl:clear-color
      (if (cfg :debug)
          (values (* 0.25 (abs (sin (clock-current-time %clock)))) 0 0 1)
          (values 0 0 0 1)))
    (gl:clear :color-buffer :depth-buffer)))

(defun update-display ()
  (with-slots (%clock %display %running-p) *state*
    (when %running-p
      (clear-screen)
      (render-frame)
      (sdl2:gl-swap-window (window %display))
      (incf (clock-frame-count %clock)))))
