(in-package #:net.mfiano.lisp.pyx)

(defgeneric %on-collision-enter (contact1 contact2)
  (:method (contact1 contact2)))

(defgeneric %on-collision-continue (contact1 contact2)
  (:method (contact1 contact2)))

(defgeneric %on-collision-exit (contact1 contact2)
  (:method (contact1 contact2)))

(defgeneric on-collision-enter (layer1-symbol layer1 layer2-symbol layer2)
  (:method (layer1-symbol layer1 layer2-symbol layer2)))

(defgeneric on-collision-continue (layer1-symbol layer1 layer2-symbol layer2)
  (:method (layer1-symbol layer1 layer2-symbol layer2)))

(defgeneric on-collision-exit (layer1-symbol layer1 layer2-symbol layer2)
  (:method (layer1-symbol layer1 layer2-symbol layer2)))

(defgeneric on-collision-picked (layer-symbol entity)
  (:method (layer-symbol entity)))

(defmacro define-collision-hook (hook (layer1 &optional layer2) &body body)
  (u:with-gensyms (layer1-symbol layer2-symbol)
    (let ((hook-types '(:enter :continue :exit :picked)))
      `(progn
         ,@(unless (find hook hook-types)
             `((error "Hook type must be one of: ~{~s~^, ~}" ',hook-types)))
         ,@(unless (symbolp layer1)
             `((error "Layer 1 of a collision hook must be a symbol: ~s."
                      ',layer1)))
         ,@(unless (symbolp layer2)
             `((error "Layer 2 of a collision hook must be a symbol: ~s."
                      ',layer2)))
         ,@(if (eq hook :picked)
               `((defmethod on-collision-picked ((,layer1-symbol (eql ',layer1))
                                                 ,layer1)
                   ,@body))
               `((defmethod ,(u:format-symbol :net.mfiano.lisp.pyx
                                              "ON-COLLISION-~a" hook)
                     ((,layer1-symbol (eql ',layer1)) ,layer1
                      (,layer2-symbol (eql ',layer2)) ,layer2)
                   (when (and ,layer1 ,layer2)
                     ,@body))))))))
