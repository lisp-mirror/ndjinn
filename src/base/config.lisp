(in-package #:pyx)

(a:define-constant +allow-screensaver+ nil)
(a:define-constant +debug+ t)
(a:define-constant +debug-interval+ nil)
(a:define-constant +delta-time+ (/ 60f0))
(a:define-constant +title+ "Pyx Engine" :test #'string=)
(a:define-constant +release+ nil)
(a:define-constant +threads+ nil)
(a:define-constant +vsync+ t)
(a:define-constant +window-width+ 1280)
(a:define-constant +window-height+ 720)

(glob:define-global-var =allow-screensaver= +allow-screensaver+)
(glob:define-global-var =debug= +debug+)
(glob:define-global-var =debug-interval= +debug-interval+)
(glob:define-global-var =delta-time= +delta-time+)
(glob:define-global-var =title= +title+)
(glob:define-global-var =release= +release+)
(glob:define-global-var =threads= +threads+)
(glob:define-global-var =vsync= +vsync+)
(glob:define-global-var =window-width= +window-width+)
(glob:define-global-var =window-height= +window-height+)

(defun reset-config ()
  (setf =allow-screensaver= +allow-screensaver+
        =debug= +debug+
        =debug-interval= +debug-interval+
        =delta-time= +delta-time+
        =title= +title+
        =release= +release+
        =threads= +threads+
        =vsync= +vsync+
        =window-width= +window-width+
        =window-height= +window-height+))

(defun load-config (&rest args)
  (reset-config)
  (u:do-plist (k v args)
    (set (a:format-symbol :pyx "=~a=" k) v)))

(defun get-config-option (key)
  (symbol-value (a:format-symbol :pyx "=~a=" key)))
