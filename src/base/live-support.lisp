(in-package #:net.mfiano.lisp.pyx)

(defmacro with-continuable (report &body body)
  `(restart-case (progn ,@body)
     (continue () :report ,report)))

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
                  (with-continuable "REPL"
                    (,(find-symbol "PROCESS-REQUESTS" :slynk) t))))
              (:swank
               `(lambda ()
                  (u:when-let ((repl (or ,(find-symbol "*EMACS-CONNECTION*"
                                                       :swank)
                                         (,(find-symbol "DEFAULT-CONNECTION"
                                                        :swank)))))
                    (with-continuable "REPL"
                      (,(find-symbol "HANDLE-REQUESTS")
                       repl t)))))
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
