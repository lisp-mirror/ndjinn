(in-package #:net.mfiano.lisp.pyx)

(defgeneric %on-collision-enter (contact1 contact2)
  (:method (contact1 contact2)))

(defgeneric %on-collision-continue (contact1 contact2)
  (:method (contact1 contact2)))

(defgeneric %on-collision-exit (contact1 contact2)
  (:method (contact1 contact2)))

(defgeneric on-collision-enter (target layer entity)
  (:method (target layer entity)))

(defgeneric on-collision-continue (target layer entity)
  (:method (target layer entity)))

(defgeneric on-collision-exit (target layer entity)
  (:method (target layer entity)))

(defgeneric on-collision-picked (target layer entity)
  (:method (target layer entity)))

(defmacro define-collision-hook (name (target &key layer) &body body)
  (u:with-gensyms (target-symbol)
    (let ((hook-types '(:enter :continue :exit :picked)))
      `(progn
         ,@(unless (find name hook-types)
             `((error "Hook type must be one of: ~{~s~^, ~}" ',hook-types)))
         ,@(unless (symbolp target)
             `((error "Target of a collision hook must be a symbol: ~s."
                      ',target)))
         ,@(unless (symbolp layer)
             `((error "Layer of a collision hook must be a symbol: ~s."
                      ',layer)))
         (defmethod ,(u:format-symbol :net.mfiano.lisp.pyx
                                      "ON-COLLISION-~a" name)
             ((,target-symbol (eql ',target)) (layer (eql ',layer)) ,target)
           ,@body)))))
