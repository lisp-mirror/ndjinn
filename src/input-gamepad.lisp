(in-package #:ndjinn)

(defstruct (gamepad
            (:predicate nil)
            (:copier nil))
  id
  instance
  name
  handle)

(defstruct (gamepad-analog-state
            (:predicate nil)
            (:copier nil))
  (x 0f0 :type u:f32)
  (y 0f0 :type u:f32)
  (deadzone 0f0 :type u:f32))

(u:define-constant +gamepad-axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y)
      (:triggers :x) (:triggers :y))
  :test #'equalp)

(u:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick-button :right-stick-button
      :left-shoulder :right-shoulder :up :down :left :right)
  :test #'equalp)

;;; Utilties

(defun get-gamepad-by-instance (data instance)
  (u:href (input-data-gamepad-instances data) instance))

(defun generate-gamepad-id (data)
  (or (pop (input-data-detached-gamepads data))
      (u:format-symbol :keyword "GAMEPAD~d"
                       (1+ (hash-table-count
                            (input-data-gamepad-instances data))))))

(defun prepare-gamepads ()
  (let ((database (resolve-system-path "gamepads.db")))
    (sdl2:game-controller-add-mappings-from-file (namestring database))
    (sdl2-ffi.functions:sdl-set-hint
     sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1")))

(defun shutdown-gamepads ()
  (u:when-let* ((data (input-data =context=))
                (instances (input-data-gamepad-instances data)))
    (u:do-hash-values (v instances)
      (sdl2:game-controller-close (gamepad-handle v)))
    (clrhash instances)))

(defun normalize-gamepad-analog-value (sub-device axis value)
  (if (eq sub-device :triggers)
      (u:map-domain 0 32767 0 1 value)
      (let ((clamped (u:clamp value -32767 32767)))
        (ecase axis
          (:x (u:map-domain -32767 32767 -1 1 clamped))
          (:y (u:map-domain -32767 32767 1 -1 clamped))))))

;;; Internal event hooks

(defun event/gamepad-attach (index)
  (when (sdl2:game-controller-p index)
    (let* ((data (input-data =context=))
           (handle (sdl2:game-controller-open index))
           (instance (sdl2:game-controller-instance-id handle))
           (id (generate-gamepad-id data))
           (gamepad (make-gamepad :id id
                                  :instance instance
                                  :name (sdl2:game-controller-name handle)
                                  :handle handle)))
      (setf (u:href (input-data-gamepad-instances data) instance) gamepad
            (u:href (input-data-gamepad-ids data) id) gamepad)
      (input-transition-in :gamepad :attach id)
      (log:debug :ndjinn "Gamepad attached: ~s" id))))

(defun event/gamepad-detach (instance)
  (let* ((data (input-data =context=))
         (instances (input-data-gamepad-instances data))
         (gamepad (u:href instances instance))
         (id (gamepad-id gamepad)))
    (sdl2:game-controller-close (gamepad-handle gamepad))
    (u:appendf (input-data-detached-gamepads data) (list id))
    (remhash id (input-data-gamepad-ids data))
    (remhash instance instances)
    (input-transition-out :gamepad :attach id)
    (log:debug :ndjinn "Gamepad detached: ~s" id)))

(defun event/gamepad-analog-motion (instance axis value)
  (destructuring-bind (sub-device axis) axis
    (let* ((data (input-data =context=))
           (states (input-data-states data))
           (gamepad (get-gamepad-by-instance data instance))
           (key (list (gamepad-id gamepad) sub-device))
           (value (normalize-gamepad-analog-value sub-device axis value)))
      (u:if-let ((state (u:href states key)))
        (ecase axis
          (:x (setf (gamepad-analog-state-x state) value))
          (:y (setf (gamepad-analog-state-y state) value)))
        (let ((analog-state (make-gamepad-analog-state)))
          (setf (u:href states key) analog-state))))))

(defun event/gamepad-button-up (instance button)
  (let* ((data (input-data =context=))
         (id (gamepad-id (get-gamepad-by-instance data instance))))
    (input-transition-out id button)
    (input-transition-out id :any)))

(defun event/gamepad-button-down (instance button)
  (let* ((data (input-data =context=))
         (id (gamepad-id (get-gamepad-by-instance data instance))))
    (input-transition-in id button)
    (input-transition-in id :any)))

;;; Interface

(defun %get-gamepad-analog/axial (args)
  (let ((data (input-data =context=)))
    (u:if-found (state (u:href (input-data-states data) args))
      (let ((x (gamepad-analog-state-x state))
            (y (gamepad-analog-state-y state))
            (deadzone (gamepad-analog-state-deadzone state)))
        (values (if (< (abs x) deadzone) 0f0 x)
                (if (< (abs y) deadzone) 0f0 y)))
      (values 0f0 0f0))))

(defun %get-gamepad-analog/radial (args)
  (let ((data (input-data =context=)))
    (u:if-found (state (u:href (input-data-states data) args))
      (let ((x (gamepad-analog-state-x state))
            (y (gamepad-analog-state-y state))
            (deadzone (gamepad-analog-state-deadzone state)))
        (v2:with-components ((v (v2:vec x y)))
          (if (< (v2:length v) deadzone)
              (values 0f0 0f0)
              (values vx vy))))
      (values 0f0 0f0))))

(defun %get-gamepad-analog/radial-scaled (args)
  (let ((data (input-data =context=)))
    (u:if-found (state (u:href (input-data-states data) args))
      (let ((x (gamepad-analog-state-x state))
            (y (gamepad-analog-state-y state))
            (deadzone (gamepad-analog-state-deadzone state)))
        (v2:with-components ((v (v2:vec x y)))
          (let ((length (v2:length v)))
            (if (< length deadzone)
                (values 0f0 0f0)
                (progn
                  (v2:scale! v
                             (v2:normalize v)
                             (/ (- length deadzone) (- 1 deadzone)))
                  (values vx vy))))))
      (values 0f0 0f0))))

(defun get-gamepad-analog (deadzone-type &rest args)
  (ecase deadzone-type
    (:axial (%get-gamepad-analog/axial args))
    (:radial (%get-gamepad-analog/radial args))
    (:radial-scaled (%get-gamepad-analog/radial-scaled args))))

(defun on-gamepad-attach (gamepad-id)
  (u:when-let* ((states (input-data-states (input-data =context=)))
                (state (u:href states (list :attach gamepad-id))))
    (input-transition-enter state)))

(defun on-gamepad-enabled (gamepad-id)
  (u:when-let* ((states (input-data-states (input-data =context=)))
                (state (u:href states (list :attach gamepad-id))))
    (input-transition-enabled state)))

(defun on-gamepad-detach (gamepad-id)
  (u:when-let* ((states (input-data-states (input-data =context=)))
                (state (u:href states (list :attach gamepad-id))))
    (input-transition-exit state)))
