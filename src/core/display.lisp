(in-package #:pyx)

(defclass display ()
  ((%window :reader window
            :initarg :window)
   (%context :accessor context
             :initform nil)
   (%resolution :reader resolution
                :initarg :resolution)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)))

(defun make-opengl-context (display)
  (sdl2:gl-set-attrs :context-major-version 4
                     :context-minor-version 3
                     :context-profile-mask 1
                     :multisamplebuffers 1
                     :multisamplesamples 4)
  (let ((context (sdl2:gl-create-context (window display))))
    (setf (context display) context)
    (apply #'gl:enable +enabled-capabilities+)
    (apply #'gl:disable +disabled-capabilities+)
    (apply #'gl:blend-func +blend-mode+)
    (gl:depth-func +depth-mode+)))

(defun make-window ()
  (sdl2:create-window :title "Pyx"
                      :w (truncate =window-width=)
                      :h (truncate =window-height=)
                      :flags '(:opengl)))

(defun make-display ()
  (sdl2:init :everything)
  (let* ((refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))
         (resolution (v2:vec =window-width= =window-height=))
         (display (make-instance 'display
                                 :window (make-window)
                                 :refresh-rate refresh-rate
                                 :resolution resolution)))
    (make-opengl-context display)
    (sdl2:gl-set-swap-interval (if =vsync= 1 0))
    (if =allow-screensaver=
        (sdl2:enable-screensaver)
        (sdl2:disable-screensaver))
    (setf (display) display)))

(defun kill-display ()
  (a:when-let ((display (display)))
    (sdl2:gl-delete-context (context display))
    (sdl2:destroy-window (window display)))
  (sdl2:sdl-quit))

(defun render (display)
  (comp::render-frame)
  (sdl2:gl-swap-window (window display))
  (incf (clock-frame-count (clock))))

(defun get-resolution ()
  (resolution (display)))
