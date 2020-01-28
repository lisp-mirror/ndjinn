(in-package #:pyx.config)

(glob:define-global-var =DEFAULT-DEBUG= t)
(glob:define-global-var =DEFAULT-DEBUG-INTERVAL= nil)
(glob:define-global-var =DEFAULT-TITLE= "Pyx Engine")
(glob:define-global-var =DEFAULT-RELEASE= nil)
(glob:define-global-var =DEFAULT-THREADS= nil)
(glob:define-global-var =DEFAULT-VSYNC= t)
(glob:define-global-var =DEFAULT-WINDOW-WIDTH= 1280)
(glob:define-global-var =DEFAULT-WINDOW-HEIGHT= 720)
(glob:define-global-var =DEFAULT-ALLOW-SCREENSAVER= nil)

(glob:define-global-var =DEBUG= =DEFAULT-DEBUG=)
(glob:define-global-var =DEBUG-INTERVAL= =DEFAULT-DEBUG-INTERVAL=)
(glob:define-global-var =TITLE= =DEFAULT-TITLE=)
(glob:define-global-var =RELEASE= =DEFAULT-RELEASE=)
(glob:define-global-var =THREADS= =DEFAULT-THREADS=)
(glob:define-global-var =VSYNC= =DEFAULT-VSYNC=)
(glob:define-global-var =WINDOW-WIDTH= =DEFAULT-WINDOW-WIDTH=)
(glob:define-global-var =WINDOW-HEIGHT= =DEFAULT-WINDOW-HEIGHT=)
(glob:define-global-var =ALLOW-SCREENSAVER= =DEFAULT-ALLOW-SCREENSAVER=)

(defun reset ()
  (setf =DEBUG= =DEFAULT-DEBUG=
        =DEBUG-INTERVAL= =DEFAULT-DEBUG-INTERVAL=
        =TITLE= =DEFAULT-TITLE=
        =RELEASE= =DEFAULT-RELEASE=
        =THREADS= =DEFAULT-THREADS=
        =VSYNC= =DEFAULT-VSYNC=
        =WINDOW-WIDTH= =DEFAULT-WINDOW-WIDTH=
        =WINDOW-HEIGHT= =DEFAULT-WINDOW-HEIGHT=
        =ALLOW-SCREENSAVER= =DEFAULT-ALLOW-SCREENSAVER=))

(defun load (&rest args)
  (reset)
  (u:do-plist (k v args)
    (set (a:format-symbol :pyx.config "=~:@(~a~)=" k) v)))
