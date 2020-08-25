(in-package #:net.mfiano.lisp.pyx)

(u:define-constant +allow-screensaver+ nil)
(u:define-constant +anti-alias+ t)
(u:define-constant +delta-time+ 1/60)
(u:define-constant +log-level+ :debug)
(u:define-constant +log-assets+ nil)
(u:define-constant +log-repl+ t)
(u:define-constant +log-repl-categories+ '(:pyx) :test #'equal)
(u:define-constant +threads+ nil)
(u:define-constant +title+ "Pyx Engine" :test #'string=)
(u:define-constant +vsync+ t)
(u:define-constant +window-height+ 720)
(u:define-constant +window-width+ 1280)

(glob:define-global-var =allow-screensaver= +allow-screensaver+)
(glob:define-global-var =anti-alias= +anti-alias+)
(glob:define-global-var =delta-time= +delta-time+)
(glob:define-global-var =log-assets= +log-assets+)
(glob:define-global-var =log-level= +log-level+)
(glob:define-global-var =log-repl= +log-repl+)
(glob:define-global-var =log-repl-categories= +log-repl-categories+)
(glob:define-global-var =threads= +threads+)
(glob:define-global-var =title= +title+)
(glob:define-global-var =vsync= +vsync+)
(glob:define-global-var =window-height= +window-height+)
(glob:define-global-var =window-width= +window-width+)

(defun reset-config ()
  (setf =allow-screensaver= +allow-screensaver+
        =anti-alias= +anti-alias+
        =delta-time= +delta-time+
        =log-assets= +log-assets+
        =log-level= +log-level+
        =log-repl= +log-repl+
        =log-repl-categories= +log-repl-categories+
        =threads= +threads+
        =title= +title+
        =vsync= +vsync+
        =window-height= +window-height+
        =window-width= +window-width+))

(defun load-config ()
  (reset-config)
  (u:when-let ((project (project)))
    (load-user-config project)))

(defun load-user-config (project)
  (let ((path (uiop:merge-pathnames*
               (make-pathname :directory `(:relative "Pyx Games" ,project)
                              :name "settings"
                              :type "conf")
               (uiop:xdg-config-home)))
        (package (package-name (symbol-package (%initial-scene =context=)))))
    (ensure-directories-exist path)
    (when (uiop:file-exists-p path)
      (u:do-plist (k v (u:safe-read-file-forms path :package package))
        (u:if-let ((option (find-symbol (format nil "=~:@(~a~)=" k)
                                        :net.mfiano.lisp.pyx)))
          (set option v)
          (unwind-protect
               (error "Invalid configuration option: ~(~a~) in ~s." k path)
            (reset-config)))))))

(defun get-config-option (key)
  (symbol-value (u:format-symbol :net.mfiano.lisp.pyx "=~a=" key)))
