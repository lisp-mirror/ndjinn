(in-package #:net.mfiano.lisp.pyx)

(u:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defstruct (mouse-motion-state (:predicate nil)
                               (:copier nil))
  relative
  (warp-x 0)
  (warp-y 0)
  (x 0)
  (y 0)
  (dx 0)
  (dy 0))

(defun on-mouse-button-up (data button)
  (button-transition-out data (list :mouse button))
  (button-transition-out data '(:mouse :any))
  (button-transition-out data '(:button :any)))

(defun on-mouse-button-down (data button)
  (button-transition-in data (list :mouse button))
  (button-transition-in data '(:mouse :any))
  (button-transition-in data '(:button :any)))

(defun on-mouse-scroll (data x y)
  (let ((states (states data)))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (data x y dx dy)
  (let ((motion-state (u:href (states data) '(:mouse :motion)))
        (relative (sdl2:relative-mouse-mode-p)))
    (unless relative
      (setf (mouse-motion-state-x motion-state) x
            (mouse-motion-state-y motion-state) (- (cfg/player :window-height)
                                                   y)))
    (setf (mouse-motion-state-dx motion-state) dx
          (mouse-motion-state-dy motion-state) (- dy))))

(defun reset-mouse-state (data)
  (let* ((states (states data))
         (motion-state (u:href states '(:mouse :motion))))
    (setf (u:href states '(:mouse :scroll-horizontal)) 0
          (u:href states '(:mouse :scroll-vertical)) 0
          (mouse-motion-state-dx motion-state) 0
          (mouse-motion-state-dy motion-state) 0)))

(defun get-mouse-position ()
  (let* ((motion-state (u:href (states (input-data =context=))
                               '(:mouse :motion)))
         (x (mouse-motion-state-x motion-state))
         (y (mouse-motion-state-y motion-state))
         (dx (mouse-motion-state-dx motion-state))
         (dy (mouse-motion-state-dy motion-state)))
    (values x y dx dy)))

(defun get-mouse-scroll (axis)
  (let ((states (states (input-data =context=))))
    (ecase axis
      (:horizontal (or (u:href states '(:mouse :scroll-horizontal)) 0))
      (:vertical (or (u:href states '(:mouse :scroll-vertical)) 0)))))

(defun enable-relative-motion ()
  (let* ((motion-state (u:href (states (input-data =context=))
                               '(:mouse :motion)))
         (x (mouse-motion-state-x motion-state))
         (y (- (cfg/player :window-height)
               (mouse-motion-state-y motion-state))))
    (sdl2:set-relative-mouse-mode 1)
    (setf (mouse-motion-state-relative motion-state) t
          (mouse-motion-state-warp-x motion-state) x
          (mouse-motion-state-warp-y motion-state) y)))

(defun disable-relative-motion (&key (warp t))
  (let ((motion-state (u:href (states (input-data =context=))
                              '(:mouse :motion))))
    (sdl2:set-relative-mouse-mode 0)
    (setf (mouse-motion-state-relative motion-state) nil)
    (when warp
      (let ((x (mouse-motion-state-warp-x motion-state))
            (y (mouse-motion-state-warp-y motion-state)))
        (sdl2:warp-mouse-in-window nil x y)))))

(defun mouse-motion-relative-p ()
  (let ((motion-state (u:href (states (input-data =context=))
                              '(:mouse :motion))))
    (mouse-motion-state-relative motion-state)))
