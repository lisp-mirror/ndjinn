(in-package #:pyx)

(a:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defstruct mouse-motion-state
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
  (let ((motion-state (u:href (states data) '(:mouse :motion))))
    (setf (mouse-motion-state-x motion-state) x
          (mouse-motion-state-y motion-state) (- (cfg :window-height) y)
          (mouse-motion-state-dx motion-state) dx
          (mouse-motion-state-dy motion-state) (- dy))))

(defun get-mouse-position ()
  (let* ((viewports (viewports (get-scene)))
         (viewport nil)
         (data (input-data *state*))
         (motion-state (u:href (states data) '(:mouse :motion)))
         (x (mouse-motion-state-x motion-state))
         (y (mouse-motion-state-y motion-state))
         (dx (mouse-motion-state-dx motion-state))
         (dy (mouse-motion-state-dy motion-state)))
    (u:do-hash-values (v (table viewports))
      (with-slots (%x %y %width %height) v
        (when (and (<= %x x (+ %x %width))
                   (<= %y y (+ %y %height)))
          (setf viewport v))))
    (values x y dx dy (or viewport (default viewports)))))

(defun get-mouse-scroll (axis)
  (let ((states (states (input-data *state*))))
    (ecase axis
      (:horizontal (u:href states '(:mouse :scroll-horizontal)))
      (:vertical (u:href states '(:mouse :scroll-vertical))))))

(defun reset-mouse-state (data)
  (let* ((states (states data))
         (motion-state (u:href states '(:mouse :motion))))
    (setf (u:href states '(:mouse :scroll-horizontal)) 0
          (u:href states '(:mouse :scroll-vertical)) 0
          (mouse-motion-state-dx motion-state) 0
          (mouse-motion-state-dy motion-state) 0)))
