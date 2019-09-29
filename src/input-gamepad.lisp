(in-package #:pyx)

(a:define-constant +gamepad-axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y)
      (:triggers :x) (:triggers :y))
  :test #'equalp)

(a:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick-button :right-stick-button
      :left-shoulder :right-shoulder :up :down :left :right)
  :test #'equalp)

(defclass gamepad ()
  ((%id :reader id
        :initarg :id)
   (%instance :reader instance
              :initarg :instance)
   (%name :reader name
          :initarg :name)
   (%handle :reader handle
            :initarg :handle)))

(defclass gamepad-analog-state ()
  ((%x :accessor x
       :initarg :x
       :initform 0.0)
   (%y :accessor y
       :initarg :y
       :initform 0.0)
   (%deadzone :reader deadzone
              :initarg :deadzone)))

(defun get-gamepad-by-instance (instance)
  (u:href (gamepad-instances (input-state *state*)) instance))

(defun generate-gamepad-id ()
  (with-slots (%gamepad-instances %detached-gamepads) (input-state *state*)
    (or (pop %detached-gamepads)
        (a:format-symbol :keyword "GAMEPAD~d"
                         (1+ (hash-table-count %gamepad-instances))))))

(defun prepare-gamepads ()
  (let ((database (resolve-asset-path "gamepads.db")))
    (sdl2:game-controller-add-mappings-from-file (namestring database))
    (sdl2-ffi.functions:sdl-set-hint
     sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1")))

(defun shutdown-gamepads ()
  (let ((instances (gamepad-instances (input-state *state*))))
    (u:do-hash-values (v instances)
      (sdl2:game-controller-close (handle v)))
    (clrhash instances)))

(defun normalize-gamepad-analog-value (sub-device axis value)
  (if (eq sub-device :triggers)
      (u:map-domain 0 32767 0 1 value)
      (let ((clamped (a:clamp value -32767 32767)))
        (ecase axis
          (:x (u:map-domain -32767 32767 -1 1 clamped))
          (:y (u:map-domain -32767 32767 1 -1 clamped))))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :axial)) analog-state)
  (with-slots (%x %y %deadzone) analog-state
    (v2:with-components ((v (v2:vec %x %y)))
      (values vx vy))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial)) analog-state)
  (with-slots (%x %y %deadzone) analog-state
    (v2:with-components ((v (v2:vec %x %y)))
      (if (< (v2:length v) %deadzone)
          (values 0.0 0.0)
          (values vx vy)))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial-scaled))
                                analog-state)
  (with-slots (%x %y %deadzone) analog-state
    (v2:with-components ((v (v2:vec %x %y)))
      (let ((length (v2:length v)))
        (if (< length %deadzone)
            (values 0.0 0.0)
            (progn
              (v2:scale! v
                         (v2:normalize v)
                         (/ (- length %deadzone) (- 1 %deadzone)))
              (values vx vy)))))))

(defun on-gamepad-attach (index)
  (with-slots (%gamepad-instances %gamepad-ids) (input-state *state*)
    (when (sdl2:game-controller-p index)
      (let* ((handle (sdl2:game-controller-open index))
             (instance (sdl2:game-controller-instance-id handle))
             (id (generate-gamepad-id))
             (gamepad (make-instance 'gamepad
                                     :id id
                                     :instance instance
                                     :name (sdl2:game-controller-name handle)
                                     :handle handle)))
        (setf (u:href %gamepad-instances instance) gamepad
              (u:href %gamepad-ids id) gamepad)
        (input-transition-in (list id :attach))))))

(defun on-gamepad-detach (instance)
  (with-slots (%gamepad-instances %gamepad-ids %detached-gamepads)
      (input-state *state*)
    (let* ((gamepad (u:href %gamepad-instances instance))
           (id (id gamepad)))
      (sdl2:game-controller-close (handle gamepad))
      (a:appendf %detached-gamepads (list id))
      (remhash id %gamepad-ids)
      (remhash instance %gamepad-instances)
      (input-transition-out (list id :attach)))))

(defun on-gamepad-analog-move (instance axis value)
  (with-slots (%states) (input-state *state*)
    (destructuring-bind (sub-device axis) axis
      (let* ((gamepad (get-gamepad-by-instance instance))
             (key (list (id gamepad) sub-device))
             (value (normalize-gamepad-analog-value sub-device axis value)))
        (symbol-macrolet ((state (u:href %states key)))
          (if (not state)
              (setf state (make-instance 'gamepad-analog-state
                                         :x 0.0 :y 0.0 :deadzone 0.0))
              (ecase axis
                (:x (setf (x state) value))
                (:y (setf (y state) value)))))))))

(defun on-gamepad-button-up (instance button)
  (let ((id (id (get-gamepad-by-instance instance))))
    (input-transition-out (list id button))
    (input-transition-out (list id :any))
    (input-transition-out '(:button :any))))

(defun on-gamepad-button-down (instance button)
  (let ((id (id (get-gamepad-by-instance instance))))
    (input-transition-in (list id button))
    (input-transition-in (list id :any))
    (input-transition-in '(:button :any))))

(defun get-gamepad-name (id)
  (let ((gamepad (u:href (gamepad-ids (input-state *state*)) id)))
    (name gamepad)))

(defun get-gamepad-analog (input)
  (u:if-found (state (u:href (states (input-state *state*)) input))
              (%get-gamepad-analog :radial-scaled state)
              (values 0.0 0.0)))
