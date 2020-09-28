(in-package #:net.mfiano.lisp.pyx)

(defstruct (display
            (:constructor %make-display)
            (:predicate nil)
            (:copier nil))
  window
  context
  (resolution (error "Window resolution unset.") :type v2:vec)
  (position (error "Window position unset.") :type v2:vec)
  (refresh-rate 60 :type fixnum))

(defun parse-opengl-version (version)
  (values-list (mapcar #'parse-integer (ss:split-sequence #\. version))))

(defun make-window (width height)
  (sdl2:create-window :title (cfg :title)
                      :w (truncate width)
                      :h (truncate height)
                      :flags '(:opengl)))

(defun configure-opengl-context ()
  (u:mvlet* ((version (cfg :opengl-version))
             (major minor (parse-opengl-version version)))
    (sdl2:gl-set-attrs :context-major-version major
                       :context-minor-version minor
                       :context-profile-mask 1
                       :doublebuffer 1
                       :multisamplebuffers (if (cfg :anti-alias) 1 0)
                       :multisamplesamples (if (cfg :anti-alias) 4 0))))

(defun make-opengl-context (display)
  (let ((context (sdl2:gl-create-context (display-window display))))
    (setf (display-context display) context)
    (apply #'gl:enable +enabled-capabilities+)
    (apply #'gl:disable +disabled-capabilities+)
    (apply #'gl:blend-func +blend-mode+)
    (gl:depth-func +depth-mode+)
    (gl:enable :multisample)
    (gl:pixel-store :unpack-alignment 1)
    (log:debug :pyx.core "Created OpenGL ~a context" (cfg :opengl-version))))

(defun make-display ()
  (v2:with-components ((r (v2:vec (cfg/player :window-width)
                                  (cfg/player :window-height))))
    (log:debug :pyx.core "Creating window (~dx~d)..." (floor rx) (floor ry))
    (sdl2:init-everything)
    (configure-opengl-context)
    (let* ((refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))
           (display (%make-display :window (make-window rx ry)
                                   :resolution r
                                   ;; TODO: Handle initial window position using
                                   ;; sdl2-ffi.functions:sdl-get-display-bounds
                                   :position (v2:vec)
                                   :refresh-rate refresh-rate)))
      (make-opengl-context display)
      (sdl2:gl-set-swap-interval (if (cfg :vsync) 1 0))
      (if (cfg/player :allow-screensaver)
          (sdl2:enable-screensaver)
          (sdl2:disable-screensaver))
      (setf (display =context=) display))))

(defun kill-display ()
  (u:when-let ((display (display =context=)))
    (sdl2:gl-delete-context (display-context display))
    (sdl2:destroy-window (display-window display))))

(defun render (display)
  (render-frame)
  (sdl2:gl-swap-window (display-window display))
  (incf (clock-frame-count (clock =context=))))

(defun window-resolution ()
  (display-resolution (display =context=)))

(defun (setf window-resolution) (value)
  (setf (display-resolution (display =context=)) value))

(defun window-position ()
  (display-position (display =context=)))

(defun (setf window-position) (value)
  (setf (display-position (display =context=)) value))
