(in-package #:net.mfiano.lisp.pyx)

(defstruct (display
            (:constructor %make-display)
            (:predicate nil)
            (:copier nil))
  (monitor 0 :type fixnum)
  (position (v2:vec) :type v2:vec)
  window
  context
  (resolution (v2:vec) :type v2:vec)
  (refresh-rate 0 :type fixnum))

(defstruct (window
            (:constructor %make-window)
            (:predicate nil)
            (:copier nil))
  handle
  (%title "Pyx" :type string)
  (%size (error "Window size unset.") :type v2:vec)
  (%position (v2:vec) :type v2:vec))

(defun parse-opengl-version (version)
  (values-list (mapcar #'parse-integer (ss:split-sequence #\. version))))

(defun make-window ()
  (v2:with-components ((s (v2:vec (cfg/player :window-width)
                                  (cfg/player :window-height))))
    (let* ((x (truncate sx))
           (y (truncate sy))
           (title (cfg :title))
           (handle (sdl2:create-window :title title
                                       :w x
                                       :h y
                                       :flags '(:opengl)))
           (window (%make-window :handle handle :%title title :%size s)))
      (log:debug :pyx "Created window (~dx~d)..." x y)
      window)))

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
  (let ((context (sdl2:gl-create-context (window-handle
                                          (display-window display)))))
    (setf (display-context display) context)
    (apply #'gl:enable +enabled-capabilities+)
    (apply #'gl:disable +disabled-capabilities+)
    (apply #'gl:blend-func +blend-mode+)
    (gl:depth-func +depth-mode+)
    (gl:pixel-store :unpack-alignment 1)
    (log:debug :pyx "Created OpenGL ~a context" (cfg :opengl-version))))

(defun set-display-properties (display)
  (u:mvlet* ((window (window-handle (display-window display)))
             (index (sdl2-ffi.functions:sdl-get-window-display-index window))
             (format width height rate (sdl2:get-current-display-mode index)))
    (setf
     (display-monitor display) (get-current-monitor)
     (display-position display) (get-display-position)
     (display-resolution display) (v2:vec width height)
     (display-refresh-rate display) rate)))

(defun make-display ()
  (sdl2:init-everything)
  (configure-opengl-context)
  (let* ((window (make-window))
         (display (%make-display :window window)))
    (setf (display =context=) display)
    (set-display-properties display)
    (make-opengl-context display)
    (sdl2:gl-set-swap-interval (if (cfg :vsync) 1 0))
    (if (cfg/player :allow-screensaver)
        (sdl2:enable-screensaver)
        (sdl2:disable-screensaver))
    display))

(defun kill-display ()
  (u:when-let ((display (display =context=)))
    (sdl2:gl-delete-context (display-context display))
    (sdl2:destroy-window (window-handle (display-window display)))))

(defun render (display)
  (render-frame)
  (sdl2:gl-swap-window (window-handle (display-window display)))
  (incf (clock-frame-count (clock =context=))))

(defun window-size ()
  (window-%size (display-window (display =context=))))

(defun (setf window-size) (value)
  (setf (window-%size (display-window (display =context=))) value))

(defun window-position ()
  (window-%position (display-window (display =context=))))

(defun (setf window-position) (value)
  (setf (window-%position (display-window (display =context=))) value))

(defun window-title ()
  (window-%title (display-window (display =context=))))

(defun (setf window-title) (value)
  (let ((window (display-window (display =context=))))
    (sdl2:set-window-title window value)
    (setf (window-%title window) value)))

(defun get-current-monitor ()
  (let ((window-handle (window-handle (display-window (display =context=)))))
    (sdl2-ffi.functions:sdl-get-window-display-index window-handle)))

(defun get-display-position ()
  (let* ((monitor (get-current-monitor))
         (rect (sdl2:get-display-bounds monitor))
         (position (v2:vec (sdl2:rect-x rect) (sdl2:rect-y rect))))
    (sdl2:free-rect rect)
    position))
