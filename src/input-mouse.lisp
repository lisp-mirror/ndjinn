(in-package #:ndjinn)

(defstruct (mouse-motion-state
            (:predicate nil)
            (:copier nil))
  (relative nil :type boolean)
  (warp-x 0 :type u:b16)
  (warp-y 0 :type u:b16)
  (x 0 :type u:b16)
  (y 0 :type u:b16)
  (dx 0 :type u:b16)
  (dy 0 :type u:b16))

(u:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

;;; Utiltities

(defun reset-mouse-state ()
  (let* ((states (input-data-states (input-data =context=)))
         (motion-state (u:href states '(:mouse :motion))))
    (setf (u:href states '(:mouse :scroll-horizontal)) 0
          (u:href states '(:mouse :scroll-vertical)) 0
          (mouse-motion-state-dx motion-state) 0
          (mouse-motion-state-dy motion-state) 0)))

;;; Internal event hooks

(defun event/mouse-button-up (button)
  (input-transition-out :mouse button)
  (input-transition-out :mouse :any))

(defun event/mouse-button-down (button)
  (input-transition-in :mouse button)
  (input-transition-in :mouse :any))

(defun event/mouse-wheel (x y)
  (let ((states (input-data-states (input-data =context=))))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))))

(defun event/mouse-motion (x y dx dy)
  (let ((motion-state (u:href (input-data-states (input-data =context=))
                              '(:mouse :motion)))
        (relative (sdl2:relative-mouse-mode-p)))
    (unless relative
      (setf (mouse-motion-state-x motion-state) x
            (mouse-motion-state-y motion-state) (floor (- (v2:y (window-size))
                                                          y))))
    (setf (mouse-motion-state-dx motion-state) dx
          (mouse-motion-state-dy motion-state) (- dy))))

;;; Interface

(defun get-mouse-position ()
  (let* ((motion-state (u:href (input-data-states (input-data =context=))
                               '(:mouse :motion)))
         (x (mouse-motion-state-x motion-state))
         (y (mouse-motion-state-y motion-state))
         (dx (mouse-motion-state-dx motion-state))
         (dy (mouse-motion-state-dy motion-state)))
    (values x y dx dy)))

(defun get-mouse-scroll (axis)
  (let ((states (input-data-states (input-data =context=))))
    (ecase axis
      (:horizontal (or (u:href states '(:mouse :scroll-horizontal)) 0))
      (:vertical (or (u:href states '(:mouse :scroll-vertical)) 0)))))

(defun enable-relative-motion ()
  (let* ((motion-state (u:href (input-data-states (input-data =context=))
                               '(:mouse :motion)))
         (x (mouse-motion-state-x motion-state))
         (y (floor (- (v2:y (window-size))
                      (mouse-motion-state-y motion-state)))))
    (sdl2:set-relative-mouse-mode 1)
    (setf (mouse-motion-state-relative motion-state) t
          (mouse-motion-state-warp-x motion-state) x
          (mouse-motion-state-warp-y motion-state) y)))

(defun disable-relative-motion (&key (warp t))
  (let ((motion-state (u:href (input-data-states (input-data =context=))
                              '(:mouse :motion))))
    (sdl2:set-relative-mouse-mode 0)
    (setf (mouse-motion-state-relative motion-state) nil)
    (when warp
      (let ((x (mouse-motion-state-warp-x motion-state))
            (y (mouse-motion-state-warp-y motion-state)))
        (sdl2:warp-mouse-in-window nil x y)))))

(defun mouse-motion-relative-p ()
  (let ((motion-state (u:href (input-data-states (input-data =context=))
                              '(:mouse :motion))))
    (mouse-motion-state-relative motion-state)))
