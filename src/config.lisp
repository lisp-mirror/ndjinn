(in-package #:ndjinn)

(defmacro define-config (name () &body body)
  (u:with-gensyms (key)
    (let ((table `(u:plist->hash ',(car body)))
          (default `(u:href =meta/config-developer= 'default)))
      (if (eq name 'default)
          `(setf (u:href =meta/config-developer= ',name) ,table)
          `(progn
             (u:do-hash-keys (,key ,table)
               (u:unless-found (#:nil (u:href ,default ,key))
                 (error "Invalid configuration option: ~s." ,key)))
             (setf (u:href =meta/config-developer= ',name)
                   (u:hash-merge ,default ,table)))))))

(defun load-player-config ()
  (u:when-let* ((project (cfg :project))
                (path (uiop:merge-pathnames*
                       (make-pathname :directory `(:relative "Ndjinn" ,project)
                                      :name "settings"
                                      :type "conf")
                       (uiop:xdg-config-home)))
                (package (package-name (symbol-package (name =context=)))))
    (ensure-directories-exist path)
    (cond
      ((uiop:file-exists-p path)
       (log:info :ndjinn "Loading player configuration from ~a" path)
       (let ((table =meta/config-player=))
         (u:do-plist (k v (u:safe-read-file-forms path :package package))
           (let ((key (u:make-keyword k)))
             (u:if-found (#:nil (u:href table 'default key))
               (progn
                 (setf (cfg/player key) v)
                 (log:info :ndjinn "Player configuration override: ~(~a~) = ~s"
                           k v))
               (log:warn :ndjinn "Invalid configuration option: ~(~a~)" k))))))
      (t
       (log:info :ndjinn "No user configuration file found at ~a" path)))))

(defun cfg (key)
  (let ((config =meta/config-developer=))
    (u:if-let ((table (u:href config (name =context=))))
      (u:href table key)
      (u:href config 'default key))))

(defun cfg/player (key)
  (let ((config =meta/config-player=))
    (u:if-let ((table (u:href config (name =context=))))
      (u:href table key)
      (u:href config 'default key))))

(defun (setf cfg/player) (value key)
  (let ((config =meta/config-player=))
    (u:if-let ((table (u:href config (name =context=))))
      (setf (u:href table key) value)
      (setf (u:href config (name =context=)) (u:dict #'eq key value)))))

(setf (u:href =meta/config-player= 'default)
      (u:dict #'eq
              :allow-screensaver nil
              :threads nil
              :window-width 1920
              :window-height 1080))
