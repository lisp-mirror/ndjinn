(in-package #:pyx)

(defclass display ()
  ((%game-state :reader game-state
                :initarg :game-state)
   (%window :accessor window)
   (%width :reader width
           :initform 800)
   (%height :reader height
            :initform 450)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)))

(defun make-opengl-context (display)
  (with-slots (%window) display
    (sdl2:gl-set-attrs :context-major-version 4
                       :context-minor-version 3
                       :context-profile-mask 1
                       :multisamplebuffers 1
                       :multisamplesamples 4)
    (sdl2:gl-create-context %window)
    (gl:enable :depth-test :blend :multisample :cull-face)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (u:noop)))

(defun make-window (display)
  (with-slots (%window) display
    (setf %window (sdl2:create-window :title "Pyx"
                                      :w (width display)
                                      :h (height display)
                                      :flags '(:opengl)))))

(defun make-display (game-state)
  (let* ((refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))
         (display (make-instance 'display
                                 :game-state game-state
                                 :refresh-rate refresh-rate)))
    (make-window display)
    (make-opengl-context display)
    (sdl2:gl-set-swap-interval 1)
    display))

(defun clear-screen (display)
  (with-slots (%debug-p %clock) (game-state display)
    (multiple-value-call #'gl:clear-color
      (if %debug-p
          (values (* 0.25 (abs (sin (clock-current-time %clock)))) 0 0 1)
          (values 0 0 0 1)))
    (gl:clear :color-buffer :depth-buffer)))

(defun update-display (game-state)
  (with-slots (%clock %display %running-p) game-state
    (when %running-p
      (clear-screen %display)
      (render-game-objects game-state)
      (sdl2:gl-swap-window (window %display))
      (incf (clock-frame-count %clock)))))
