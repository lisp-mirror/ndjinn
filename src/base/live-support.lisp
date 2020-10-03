(in-package #:net.mfiano.lisp.pyx)

(defmacro with-continuable (&body body)
  (u:with-gensyms (debugger-entry-time previous-hook pause-time)
    (let ((hook #+sbcl 'sb-ext:*invoke-debugger-hook*
                #-sbcl '*debugger-hook*))
      `(let* ((,debugger-entry-time nil)
              (,previous-hook ,hook)
              (,hook
                (lambda (condition hook)
                  (declare (ignore hook))
                  (log:debug :pyx.core "Entered debugger")
                  (setf ,debugger-entry-time (get-time))
                  (when ,previous-hook
                    (funcall ,previous-hook condition ,previous-hook)))))
         (restart-case (progn ,@body)
           (abort ()
             :report "Pyx: Skip processing the currently executing frame"
             (when ,debugger-entry-time
               (let ((,pause-time (- (get-time) ,debugger-entry-time)))
                 (incf (pause-time) ,pause-time)
                 (log:debug :pyx.core "Spent ~3$ seconds in the debugger"
                            ,pause-time)))))))))

(flet ((generate-live-support-functions ()
         (let ((repl-package (find-if #'find-package '(:slynk :swank))))
           (compile
            'setup-repl
            (if (eq repl-package :slynk)
                `(lambda ()
                   (,(find-symbol "SEND-PROMPT" :slynk-mrepl)))
                (constantly nil)))
           (compile
            'update-repl
            (case repl-package
              (:slynk
               `(lambda ()
                  (,(find-symbol "PROCESS-REQUESTS" :slynk) t)))
              (:swank
               `(lambda ()
                  (u:when-let ((repl (or ,(find-symbol "*EMACS-CONNECTION*"
                                                       :swank)
                                         (,(find-symbol "DEFAULT-CONNECTION"
                                                        :swank)))))
                    (,(find-symbol "HANDLE-REQUESTS" :swank) repl t))))
              (t (constantly nil))))
           (compile
            'send-to-repl
            (if (eq repl-package :slynk)
                `(lambda (values &key (comment "Sent from Pyx"))
                   (,(find-symbol "COPY-TO-REPL-IN-EMACS" :slynk-mrepl)
                    values :blurb comment :pop-to-buffer nil))
                (constantly nil))))))
  (generate-live-support-functions))

(defgeneric recompile (type data)
  (:method (type data)
    (warn "No live recompilation hook defined for spec type: ~s" type)))

(defmacro on-recompile (type data () &body body)
  (u:with-gensyms (purpose event-type)
    `(progn
       (defmethod recompile ((type (eql ',type)) ,data)
         (declare (ignorable ,data))
         ,@body)
       (defmethod handle-queued-event ((,purpose (eql :recompile))
                                       (,event-type (eql ',type))
                                       ,data)
         (funcall #'recompile ,event-type ,data)))))
