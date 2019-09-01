(in-package #:pyx)

(defmacro with-continue-restart (report &body body)
  (a:with-gensyms (clock)
    `(let* ((,clock (clock *game-state*))
            (debugger-entry-time)
            (previous-hook *debugger-hook*)
            (#+sbcl sb-ext:*invoke-debugger-hook*
             #-sbcl *debugger-hook*
             (lambda (condition hook)
               (setf debugger-entry-time (get-time ,clock))
               (when previous-hook
                 (funcall previous-hook condition hook)))))
       (restart-case (progn ,@body)
         (continue ()
           :report ,report
           (when debugger-entry-time
             (setf (clock-pause-time ,clock)
                   (- (get-time ,clock) debugger-entry-time))))))))

(defun compile-live-coding-functions ()
  (let ((repl-package (find-if #'find-package '(:slynk :swank))))
    (macrolet ((sym (sym &optional package)
                 (let ((name (symbol-name sym)))
                   `(a:ensure-symbol ,name ,(or package 'repl-package)))))
      (case repl-package
        ((:slynk :swank)
         (compile 'find-lisp-repl
                  `(lambda ()
                     (or ,(sym :*emacs-connection*)
                         (,(sym :default-connection)))))
         (compile 'setup-lisp-repl
                  (ecase repl-package
                    (:slynk
                     `(lambda ()
                        (a:when-let ((repl (find
                                            (,(sym :current-thread))
                                            (,(sym :channels))
                                            :key #',(sym :channel-thread))))
                          (,(sym :send-prompt :slynk-mrepl) repl))))
                    (:swank
                     (constantly nil))))
         (compile 'update-lisp-repl
                  `(lambda ()
                     (a:when-let ((repl (find-lisp-repl)))
                       (with-continue-restart "REPL"
                         (,(sym :handle-requests) repl t))))))
        (t (setf (symbol-function 'setup-lisp-repl) (constantly nil)
                 (symbol-function 'update-lisp-repl) (constantly nil)))))))

(defun setup-live-coding ()
  (compile-live-coding-functions)
  (funcall 'setup-lisp-repl))

(defun live-coding-update ()
  (funcall 'update-lisp-repl))
