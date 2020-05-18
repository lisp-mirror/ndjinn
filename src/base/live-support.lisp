(in-package #:net.mfiano.lisp.pyx)

(defmacro with-continuable (report &body body)
  `(restart-case (progn ,@body)
     (continue () :report ,report)))

(defun compile-repl-functions ()
  (let ((repl-package (find-if #'find-package '(:slynk :swank))))
    (macrolet ((sym (sym &optional package)
                 (let ((name (symbol-name sym)))
                   `(u:ensure-symbol ,name ,(or package 'repl-package)))))
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
                        (u:when-let ((repl (find
                                            (,(sym :current-thread))
                                            (,(sym :channels))
                                            :key #',(sym :channel-thread))))
                          (,(sym :send-prompt :slynk-mrepl) repl))))
                    (:swank
                     (constantly nil))))
         (compile '%update-repl
                  `(lambda ()
                     (u:when-let ((repl (%find-repl)))
                       (with-continuable "REPL"
                         (,(sym :handle-requests) repl t))))))
        (t (setf (symbol-function '%setup-repl) (constantly nil)
                 (symbol-function '%update-repl) (constantly nil)))))))

(defun setup-repl ()
  (compile-repl-functions)
  (funcall '%setup-repl))

(defun update-repl ()
  (funcall '%update-repl))

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
