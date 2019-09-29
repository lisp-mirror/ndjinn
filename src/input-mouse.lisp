(in-package #:pyx)

(a:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defclass mouse-motion-state ()
  ((%x :accessor x)
   (%y :accessor y)
   (%dx :accessor dx)
   (%dy :accessor dy)))

(defun on-mouse-button-up (button)
  (input-transition-out (list :mouse button))
  (input-transition-out '(:mouse :any))
  (input-transition-out '(:button :any)))

(defun on-mouse-button-down (button)
  (input-transition-in (list :mouse button))
  (input-transition-in '(:mouse :any))
  (input-transition-in '(:button :any)))

(defun on-mouse-scroll (x y)
  (let* ((input-state (input-state *state*))
         (states (states input-state)))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (x y dx dy)
  (let* ((input-state (input-state *state*))
         (motion-states (u:href (states input-state) '(:mouse :motion))))
    (setf (x motion-states) x
          (y motion-states) y
          (dx motion-states) dx
          (dy motion-states) dy)))
