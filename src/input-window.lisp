(in-package #:pyx)

(a:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore
      :mouse-focus-enter :mouse-focus-exit :keyboard-focus-enter
      :keyboard-focus-exit :close nil nil)
  :test #'equalp)

(defun on-window-show (game-state)
  (declare (ignore game-state)))

(defun on-window-hide (game-state)
  (declare (ignore game-state)))

(defun on-window-move (game-state &key x y)
  (declare (ignore game-state x y)))

(defun on-window-resize (game-state &key width height)
  (declare (ignore game-state width height)))

(defun on-window-minimize (game-state)
  (declare (ignore game-state)))

(defun on-window-maximize (game-state)
  (declare (ignore game-state)))

(defun on-window-restore (game-state)
  (declare (ignore game-state)))

(defun on-window-mouse-focus-enter (game-state)
  (declare (ignore game-state)))

(defun on-window-mouse-focus-leave (game-state)
  (declare (ignore game-state)))

(defun on-window-keyboard-focus-enter (game-state)
  (declare (ignore game-state)))

(defun on-window-keyboard-focus-leave (game-state)
  (declare (ignore game-state)))

(defun on-window-close (game-state)
  (declare (ignore game-state)))
