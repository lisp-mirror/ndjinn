(in-package #:net.mfiano.lisp.pyx)

(u:define-constant +allow-screensaver+ nil)
(u:define-constant +anti-alias+ t)
(u:define-constant +delta-time+ 1/60)
(u:define-constant +log-assets+ nil)
(u:define-constant +log-repl-level+ :debug)
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
(glob:define-global-var =log-repl-level= +log-repl-level+)
(glob:define-global-var =log-repl-categories= +log-repl-categories+)
(glob:define-global-var =threads= +threads+)
(glob:define-global-var =title= +title+)
(glob:define-global-var =vsync= +vsync+)
(glob:define-global-var =window-height= +window-height+)
(glob:define-global-var =window-width= +window-width+)

(u:define-constant +user-options+
    '(:threads
      :window-height
      :window-width)
  :test #'equal)

(u:define-constant +developer-options+
    (append +user-options+
            '(:allow-screensaver
              :anti-alias
              :delta-time
              :log-assets
              :log-repl-level
              :log-repl-categories
              :title
              :vsync))
  :test #'equal)

(defun reset-config ()
  (setf =allow-screensaver= +allow-screensaver+
        =anti-alias= +anti-alias+
        =delta-time= +delta-time+
        =log-assets= +log-assets+
        =log-repl-level= +log-repl-level+
        =log-repl-categories= +log-repl-categories+
        =threads= +threads+
        =title= +title+
        =vsync= +vsync+
        =window-height= +window-height+
        =window-width= +window-width+))

(defun load-developer-config ()
  (reset-config)
  (u:do-plist (k v (u:href (metadata-config =metadata=) (name =context=)))
    (if (find k +developer-options+)
        (let ((option (u:format-symbol :net.mfiano.lisp.pyx "=~a=" k)))
          (set option v))
        (error "Invalid configuration option: ~(~a~)" k))))

(defun load-user-config ()
  (u:when-let* ((project (project =context=))
                (path (uiop:merge-pathnames*
                       (make-pathname :directory `(:relative "Pyx Games"
                                                             ,project)
                                      :name "settings"
                                      :type "conf")
                       (uiop:xdg-config-home)))
                (package (package-name
                          (symbol-package
                           (initial-scene =context=)))))
    (ensure-directories-exist path)
    (cond
      ((uiop:file-exists-p path)
       (log:info :pyx.cfg "Loading user configuration from ~a" path)
       (u:do-plist (k v (u:safe-read-file-forms path :package package))
         (if (find (u:make-keyword k) +user-options+)
             (let ((option (u:format-symbol :net.mfiano.lisp.pyx "=~a=" k)))
               (set option v)
               (log:info :pyx.cfg "User configuration override: ~(~a~) = ~s"
                          k v))
             (log:warn :pyx.cfg "Invalid configuration option: ~(~a~)" k))))
      (t
       (log:info :pyx.cfg "No user configuration file found at ~a" path)))))

(defmacro define-config (context () &body body)
  (u:with-gensyms (key)
    (let ((keys (u:plist-keys (car body))))
      `(if (every (lambda (,key) (find ,key +developer-options+)) ',keys)
           (setf (u:href (metadata-config =metadata=) ',context) ',@body)
           (error "Invalid configuration options: ~{~s~^, ~}."
                  (sort (set-difference ',keys +developer-options+)
                        #'string<))))))
