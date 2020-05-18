(in-package #:net.mfiano.lisp.pyx)

(u:define-constant +allow-screensaver+ nil)
(u:define-constant +anti-alias+ t)
(u:define-constant +debug+ t)
(u:define-constant +debug-interval+ nil)
(u:define-constant +delta-time+ 1/60)
(u:define-constant +title+ "Pyx Engine" :test #'string=)
(u:define-constant +release+ nil)
(u:define-constant +threads+ nil)
(u:define-constant +vsync+ t)
(u:define-constant +window-width+ 1280)
(u:define-constant +window-height+ 720)

(glob:define-global-var =allow-screensaver= +allow-screensaver+)
(glob:define-global-var =anti-alias= +anti-alias+)
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
        =anti-alias= +anti-alias+
        =debug= +debug+
        =debug-interval= +debug-interval+
        =delta-time= +delta-time+
        =title= +title+
        =release= +release+
        =threads= +threads+
        =vsync= +vsync+
        =window-width= +window-width+
        =window-height= +window-height+))

(defun load-config ()
  (reset-config)
  (u:do-plist (k v (get-context-config *context*))
    (set (u:format-symbol :net.mfiano.lisp.pyx "=~a=" k) v)))

(defun get-config-option (key)
  (symbol-value (u:format-symbol :net.mfiano.lisp.pyx "=~a=" key)))
