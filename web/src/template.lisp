(in-package #:pyx.web)

(defvar *templates* (u:dict))

(defun render (view &rest args)
  (let ((djula:*fancy-error-template-p* nil)
        (djula:*catch-template-errors-p* t))
    (symbol-macrolet ((template (u:href *templates* view)))
      (unless template
        (let ((template-name (format nil "~(~a~).html" view)))
          (setf template (djula:compile-template* template-name))))
      (apply #'djula:render-template*
             template
             nil
             :css-local (cfg :css.local)
             :css-remote (cfg :css.remote)
             :js-local (cfg :js.local)
             :js-remote (cfg :js.remote)
             :site-title (cfg :site.title)
             :site-author (cfg :site.author)
             :site-description (cfg :site.description)
             :site-url (cfg :site.url)
             :site-admin (cfg :site.admin)
             :site-year (cfg :site.year)
             :csrf-token (csrf-token)
             :user-name (session-value :user-name)
             :current-year (format-time '(:year))
             :messages (get-messages)
             (append (a:hash-table-plist *config*) args)))))
