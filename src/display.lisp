(in-package #:pyx)

(defclass display ()
  ((%window :accessor window)
   (%resolution :reader resolution
                :initarg :resolution)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)))

(defun get-window-resolution ()
  (resolution (display *state*)))

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

(defun make-window (display width height)
  (with-slots (%window) display
    (setf %window (sdl2:create-window :title "Pyx"
                                      :w (truncate width)
                                      :h (truncate height)
                                      :flags '(:opengl)))))

(defun make-display ()
  (sdl2:init :everything)
  (let* ((refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))
         (width (cfg :window-width))
         (height (cfg :window-height))
         (display (make-instance 'display
                                 :refresh-rate refresh-rate
                                 :resolution (v2:vec width height))))
    (make-window display width height)
    (make-opengl-context display)
    (sdl2:gl-set-swap-interval (if (cfg :vsync) 1 0))
    (if (cfg :allow-screensaver)
        (sdl2:enable-screensaver)
        (sdl2:disable-screensaver))
    (setf (slot-value *state* '%display) display)
    display))

(defun kill-display ()
  (sdl2:destroy-window (window (display *state*)))
  (sdl2:sdl-quit))

(defun update-display (render-func)
  (with-slots (%clock %display) *state*
    (funcall render-func)
    (sdl2:gl-swap-window (window %display))
    (incf (clock-frame-count %clock))))
