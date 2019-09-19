(in-package #:pyx)

(defclass input-state ()
  ((%entering :accessor entering
              :initform nil)
   (%exiting :accessor exiting
             :initform nil)
   (%states :reader states
            :initform (u:dict #'equal
                              '(:mouse :motion) (make-instance 'mouse-motion-state)
                              '(:mouse :scroll-horizontal) 0
                              '(:mouse :scroll-vertical) 0))))

(defclass input-button-states ()
  ((%enter :accessor enter
           :initarg :enter
           :initform nil)
   (%enabled :accessor enabled
             :initarg :enabled
             :initform nil)
   (%exit :accessor exit)))

(defun input-transition-in (input-state input)
  (symbol-macrolet ((state (u:href (states input-state) input)))
    (with-slots (%enter %enabled %exit) state
      (if state
          (setf %enter t
                %enabled t
                %exit nil)
          (setf state (make-instance 'input-button-states :enter t :enabled t)))
      (push input (entering input-state)))))

(defun input-transition-out (input-state input)
  (a:when-let ((state (u:href (states input-state) input)))
    (with-slots (%enter %enabled %exit) state
      (setf %enter nil
            %enabled nil
            %exit t)
      (push input (exiting input-state)))))

(defun input-enable-entering (input-state)
  (symbol-macrolet ((entering (entering input-state)))
    (dolist (input entering)
      (with-slots (%enter %enabled %exit) (u:href (states input-state) input)
        (setf %enter nil
              %enabled t
              %exit nil)))
    (setf entering nil)))

(defun input-disable-exiting (input-state)
  (symbol-macrolet ((exiting (exiting input-state)))
    (dolist (input exiting)
      (with-slots (%enter %enabled %exit) (u:href (states input-state) input)
        (setf %enter nil
              %enabled nil
              %exit nil)))
    (setf exiting nil)))

(defun input-enter-p (&rest args)
  (a:when-let* ((input-state (input-state *state*))
                (state (u:href (states input-state) args)))
    (enter state)))

(defun input-enabled-p (&rest args)
  (a:when-let* ((input-state (input-state *state*))
                (state (u:href (states input-state) args)))
    (enabled state)))

(defun input-exit-p (&rest args)
  (a:when-let* ((input-state (input-state *state*))
                (state (u:href (states input-state) args)))
    (exit state)))

(defmacro event-case ((event) &body handlers)
  (let (events)
    (dolist (handler handlers)
      (destructuring-bind (type options . body) handler
        (let ((body (list*
                     `(declare (ignorable ,@(u:plist-values options)))
                     body)))
          (dolist (type (a:ensure-list type))
            (a:when-let ((x (sdl2::expand-handler event type options body)))
              (push x events))))))
    `(case (sdl2:get-event-type ,event)
       ,@(nreverse events))))

(defun dispatch-event (event)
  (event-case (event)
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (on-window-show))
       (:hide (on-window-hide))
       (:move (on-window-move :x data1 :y data2))
       (:resize (on-window-resize :width data1 :height data2))
       (:minimize (on-window-minimize))
       (:maximize (on-window-maximize))
       (:restore (on-window-restore))
       (:mouse-focus-enter (on-window-mouse-focus-enter))
       (:mouse-focus-leave (on-window-mouse-focus-leave))
       (:keyboard-focus-enter (on-window-keyboard-focus-enter))
       (:keyboard-focus-leave (on-window-keyboard-focus-leave))
       (:close (on-window-close))))
    (:mousebuttonup
     (:button button)
     (on-mouse-button-up (aref +mouse-button-names+ button)))
    (:mousebuttondown
     (:button button)
     (on-mouse-button-down (aref +mouse-button-names+ button)))
    (:mousewheel
     (:x x :y y)
     (on-mouse-scroll x y))
    (:mousemotion
     (:x x :y y :xrel dx :yrel dy)
     (on-mouse-move x y dx dy))
    (:keyup
     (:keysym keysym)
     (on-key-up (aref +key-names+ (sdl2:scancode-value keysym))))
    (:keydown
     (:keysym keysym)
     (on-key-down (aref +key-names+ (sdl2:scancode-value keysym))))
    ;; TODO: gamepad support
    (:controllerdeviceadded
     (:which gamepad-id))
    (:controllerdeviceremoved
     (:which gamepad-id))
    (:controlleraxismotion
     (:which gamepad-id :axis axis :value value))
    (:controllerbuttonup
     (:which gamepad-id :button button))
    (:controllerbuttondown
     (:which gamepad-id :button button))))

(defun perform-input-tasks ()
  (let* ((input-state (input-state *state*))
         (states (states input-state)))
    (setf (u:href states '(:mouse :scroll-horizontal)) 0
          (u:href states '(:mouse :scroll-vertical)) 0)
    (input-enable-entering input-state)
    (input-disable-exiting input-state)))

(defun handle-events ()
  (let ((event (sdl2:new-event)))
    (unwind-protect
         (loop :until (zerop (sdl2:next-event event :poll))
               :do (dispatch-event event))
      (sdl2:free-event event))))
