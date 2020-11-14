(in-package #:ndjinn)

(defmacro with-continuable (&body body)
  (u:with-gensyms (debugger-entry-time previous-hook pause-time)
    (let ((hook #+sbcl 'sb-ext:*invoke-debugger-hook*
                #-sbcl '*debugger-hook*))
      `(let* ((,previous-hook ,hook)
              (,hook
                (lambda (condition hook)
                  (declare (ignore hook))
                  (let ((,debugger-entry-time (get-time)))
                    (log:debug :ndjinn "Entered debugger")
                    (unwind-protect
                         (when ,previous-hook
                           (funcall ,previous-hook condition ,previous-hook))
                      (when ,debugger-entry-time
                        (let ((,pause-time (- (get-time) ,debugger-entry-time)))
                          (incf (pause-time) ,pause-time)
                          (log:debug :ndjinn
                                     "Foo: Spent ~3$ seconds in the debugger"
                                     ,pause-time))))))))
         (restart-case (progn ,@body)
           (abort ()
             :report "Ndjinn: Skip processing current frame"))))))

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
                  (let ((before-time (get-time)))
                    (,(find-symbol "PROCESS-REQUESTS" :slynk) t)
                    (incf (pause-time) (- (get-time) before-time)))))
              (:swank
               `(lambda ()
                  (u:when-let ((repl (or ,(find-symbol "*EMACS-CONNECTION*"
                                                       :swank)
                                         (,(find-symbol "DEFAULT-CONNECTION"
                                                        :swank))))
                               (before-time (get-time)))
                    (,(find-symbol "HANDLE-REQUESTS" :swank) repl t)
                    (incf (pause-time) (- (get-time) before-time)))))
              (t (constantly nil))))
           (compile
            'send-to-repl
            (if (eq repl-package :slynk)
                `(lambda (values &key (comment "Sent from Ndjinn"))
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
