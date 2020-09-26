(in-package #:net.mfiano.lisp.pyx)

(u:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore
      :mouse-focus-enter :mouse-focus-exit :keyboard-focus-enter
      :keyboard-focus-exit :close nil nil)
  :test #'equalp)

(defgeneric on-window-show (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-hide (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-move (context &key)
  (:method (context &key x y)
    (declare (ignore x y)))
  (:method :before (context &key x y)
    (setf (window-position) (v2:vec x y))))

(defgeneric on-window-resize (context &key width height)
  (:method (context &key width height)
    (declare (ignore width height)))
  (:method :before (context &key width height)
    (setf (window-resolution) (v2:vec width height))))

(defgeneric on-window-minimize (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-maximize (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-restore (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-mouse-focus-enter (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-mouse-focus-leave (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-keyboard-focus-enter (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-keyboard-focus-leave (context)
  (:method (context))
  (:method :before (context)))

(defgeneric on-window-close (context)
  (:method (context))
  (:method :before (context)))
