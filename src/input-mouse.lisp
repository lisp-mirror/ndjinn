(in-package #:pyx)

(a:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defclass mouse-motion-state ()
  ((%x :accessor x)
   (%y :accessor y)
   (%dx :accessor dx)
   (%dy :accessor dy)))

(defun on-mouse-button-up (game-state button)
  (let ((input-state (input-state game-state)))
    (input-transition-out input-state (list :mouse button))
    (input-transition-out input-state '(:mouse :any))
    (input-transition-out input-state '(:button :any))))

(defun on-mouse-button-down (game-state button)
  (let ((input-state (input-state game-state)))
    (input-transition-in input-state (list :mouse button))
    (input-transition-in input-state '(:mouse :any))
    (input-transition-in input-state '(:button :any))))

(defun on-mouse-scroll (game-state x y)
  (let* ((input-state (input-state game-state))
         (states (states input-state)))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (game-state x y dx dy)
  (let ((input-state (input-state game-state)))
    (with-slots (%x %y %dx %dy) (u:href (states input-state) '(:mouse :motion))
      (setf %x x
            %y y
            %dx dx
            %dy dy))))
