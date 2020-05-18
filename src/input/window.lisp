(in-package #:net.mfiano.lisp.pyx)

(u:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore
      :mouse-focus-enter :mouse-focus-exit :keyboard-focus-enter
      :keyboard-focus-exit :close nil nil)
  :test #'equalp)

(defun on-window-show ())

(defun on-window-hide ())

(defun on-window-move (&key x y)
  (declare (ignore x y)))

(defun on-window-resize (&key width height)
  (declare (ignore width height)))

(defun on-window-minimize ())

(defun on-window-maximize ())

(defun on-window-restore ())

(defun on-window-mouse-focus-enter ())

(defun on-window-mouse-focus-leave ())

(defun on-window-keyboard-focus-enter ())

(defun on-window-keyboard-focus-leave ())

(defun on-window-close ())
