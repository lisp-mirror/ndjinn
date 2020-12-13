(in-package #:ndjinn)

(defmacro event-case ((event) &body handlers)
  (let (events)
    (dolist (handler handlers)
      (destructuring-bind (type options . body) handler
        (let ((body (list*
                     `(declare (ignorable ,@(u:plist-values options)))
                     body)))
          (dolist (type (u:ensure-list type))
            (u:when-let ((x (sdl2:expand-handler event type options body)))
              (push x events))))))
    `(case (sdl2:get-event-type ,event)
       ,@(nreverse events))))

(defun dispatch-event (event)
  (event-case (event)
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (event/window-show))
       (:hide (event/window-hide))
       (:move (event/window-move data1 data2))
       (:resize (event/window-resize data1 data2))
       (:minimize (event/window-minimize))
       (:maximize (event/window-maximize))
       (:restore (event/window-restore))
       (:mouse-focus-enter (event/window-mouse-focus-enter))
       (:mouse-focus-exit (event/window-mouse-focus-exit))
       (:keyboard-focus-enter (event/window-keyboard-focus-enter))
       (:keyboard-focus-exit (event/window-keyboard-focus-exit))
       (:close (event/window-close))))
    (:mousebuttonup
     (:button button)
     (event/mouse-button-up (aref +mouse-button-names+ button)))
    (:mousebuttondown
     (:button button)
     (event/mouse-button-down (aref +mouse-button-names+ button)))
    (:mousewheel
     (:x x :y y)
     (event/mouse-wheel x y))
    (:mousemotion
     (:x x :y y :xrel dx :yrel dy)
     (event/mouse-motion x y dx dy))
    (:keyup
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (event/key-up (aref +key-names+ (sdl2:scancode-value keysym)))))
    (:keydown
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (event/key-down (aref +key-names+ (sdl2:scancode-value keysym)))))
    (:controllerdeviceadded
     (:which gamepad-id)
     (event/gamepad-attach gamepad-id))
    (:controllerdeviceremoved
     (:which gamepad-id)
     (event/gamepad-detach gamepad-id))
    (:controlleraxismotion
     (:which gamepad-id :axis axis :value value)
     (event/gamepad-analog-motion gamepad-id
                                  (aref +gamepad-axis-names+ axis)
                                  value))
    (:controllerbuttonup
     (:which gamepad-id :button button)
     (event/gamepad-button-up gamepad-id (aref +gamepad-button-names+ button)))
    (:controllerbuttondown
     (:which gamepad-id :button button)
     (event/gamepad-button-down gamepad-id
                                (aref +gamepad-button-names+ button)))))

(defun handle-events ()
  (input-transition-enable-entering)
  (input-transition-disable-exiting)
  (reset-mouse-state)
  (loop :with event = (sdl2:new-event)
        :until (zerop (sdl2:next-event event :poll))
        :do (dispatch-event event)
        :finally (sdl2:free-event event)))
