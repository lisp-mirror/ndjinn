(in-package #:pyx)

(defmacro with-continue-restart (report &body body)
  (a:with-gensyms (clock)
    `(let* ((,clock (clock *state*))
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

(defun compile-repl-functions ()
  (let ((repl-package (find-if #'find-package '(:slynk :swank))))
    (macrolet ((sym (sym &optional package)
                 (let ((name (symbol-name sym)))
                   `(a:ensure-symbol ,name ,(or package 'repl-package)))))
      (case repl-package
        ((:slynk :swank)
         (compile '%find-repl
                  `(lambda ()
                     (or ,(sym :*emacs-connection*)
                         (,(sym :default-connection)))))
         (compile '%setup-repl
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
         (compile '%update-repl
                  `(lambda ()
                     (a:when-let ((repl (%find-repl)))
                       (with-continue-restart "REPL"
                         (,(sym :handle-requests) repl t))))))
        (t (setf (symbol-function '%setup-repl) (constantly nil)
                 (symbol-function '%update-repl) (constantly nil)))))))

(defun setup-repl ()
  (compile-repl-functions)
  (funcall '%setup-repl))

(defun update-repl ()
  (funcall '%update-repl))

(defmethod handle-queued-event ((purpose (eql :recompile)) event-type data)
  (case event-type
    (:shader (recompile-shaders data))
    (:material (update-materials data))
    (t (unhandled-queue-event-type purpose event-type))))
