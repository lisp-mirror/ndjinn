(in-package #:pyx)

(defclass game-object ()
  ((%transform :reader transform)))

;;; TODO: finish these

(defun update-game-objects (game-state)
  (interpolate-transforms game-state))

(defun render-game-objects (game-state)
  (declare (ignore game-state)))
