(in-package #:%pyx.display)

(defstruct (display (:constructor %make-display)
                    (:conc-name nil)
                    (:predicate nil)
                    (:copier nil))
  window
  (resolution (v2:vec cfg:=window-width= cfg:=window-height=))
  refresh-rate)

(defun make-opengl-context (display)
  (sdl2:gl-set-attrs :context-major-version 4
                     :context-minor-version 5
                     :context-profile-mask 1
                     :multisamplebuffers 1
                     :multisamplesamples 4)
  (sdl2:gl-create-context (window display))
  (apply #'gl:enable ogl:+enabled+)
  (apply #'gl:disable ogl:+disabled+)
  (apply #'gl:blend-func ogl:+blend-mode+)
  (gl:depth-func ogl:+depth-mode+))

(defun make-window ()
  (sdl2:create-window :title "Pyx"
                      :w (truncate cfg:=window-width=)
                      :h (truncate cfg:=window-height=)
                      :flags '(:opengl)))

(defun make-display ()
  (sdl2:init :everything)
  (let* ((refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))
         (resolution (v2:vec cfg:=window-width= cfg:=window-height=))
         (display (%make-display :window (make-window)
                                 :refresh-rate refresh-rate
                                 :resolution resolution)))
    (make-opengl-context display)
    (sdl2:gl-set-swap-interval (if cfg:=vsync= 1 0))
    (if cfg:=allow-screensaver=
        (sdl2:enable-screensaver)
        (sdl2:disable-screensaver))
    (setf (ctx:display) display)))

(defun kill ()
  (sdl2:destroy-window (window (ctx:display)))
  (sdl2:sdl-quit))

(defun render (display)
  (c/render:render-frame)
  (sdl2:gl-swap-window (window display))
  (incf (clock:frame-count (ctx:clock))))

(defun get-resolution ()
  (resolution (ctx:display)))
