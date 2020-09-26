(in-package #:net.mfiano.lisp.pyx)

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

(defun dispatch-event (data event)
  (event-case (event)
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (on-window-show =context=))
       (:hide (on-window-hide =context=))
       (:move (on-window-move =context= :x data1 :y data2))
       (:resize (on-window-resize =context= :width data1 :height data2))
       (:minimize (on-window-minimize =context=))
       (:maximize (on-window-maximize =context=))
       (:restore (on-window-restore =context=))
       (:mouse-focus-enter (on-window-mouse-focus-enter =context=))
       (:mouse-focus-leave (on-window-mouse-focus-leave =context=))
       (:keyboard-focus-enter (on-window-keyboard-focus-enter =context=))
       (:keyboard-focus-leave (on-window-keyboard-focus-leave =context=))
       (:close (on-window-close =context=))))
    (:mousebuttonup
     (:button button)
     (on-mouse-button-up data (aref +mouse-button-names+ button)))
    (:mousebuttondown
     (:button button)
     (on-mouse-button-down data (aref +mouse-button-names+ button)))
    (:mousewheel
     (:x x :y y)
     (on-mouse-scroll data x y))
    (:mousemotion
     (:x x :y y :xrel dx :yrel dy)
     (on-mouse-move data x y dx dy))
    (:keyup
     (:keysym keysym)
     (on-key-up data (aref +key-names+ (sdl2:scancode-value keysym))))
    (:keydown
     (:keysym keysym)
     (on-key-down data (aref +key-names+ (sdl2:scancode-value keysym))))
    (:controllerdeviceadded
     (:which gamepad-id)
     (%on-gamepad-attach data gamepad-id))
    (:controllerdeviceremoved
     (:which gamepad-id)
     (%on-gamepad-detach data gamepad-id))
    (:controlleraxismotion
     (:which gamepad-id :axis axis :value value)
     (on-gamepad-analog-move
      data gamepad-id (aref +gamepad-axis-names+ axis) value))
    (:controllerbuttonup
     (:which gamepad-id :button button)
     (on-gamepad-button-up
      data gamepad-id (aref +gamepad-button-names+ button)))
    (:controllerbuttondown
     (:which gamepad-id :button button)
     (on-gamepad-button-down
      data gamepad-id (aref +gamepad-button-names+ button)))))

(defun perform-input-tasks ()
  (let ((data (input-data =context=)))
    (button-enable-entering data)
    (button-disable-exiting data)
    (gamepad-enable-entering data)
    (gamepad-disable-exiting data)
    (reset-mouse-state data)))

(defun handle-events (data)
  (perform-input-tasks)
  (loop :with event = (sdl2:new-event)
        :until (zerop (sdl2:next-event event :poll))
        :do (dispatch-event data event)
        :finally (sdl2:free-event event)))
