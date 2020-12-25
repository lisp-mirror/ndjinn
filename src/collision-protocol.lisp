(in-package #:ndjinn)

(defgeneric on-collision-enter (layer1-symbol layer1 layer2-symbol layer2)
  (:method (layer1-symbol layer1 layer2-symbol layer2)))

(defgeneric on-collision-continue (layer1-symbol layer1 layer2-symbol layer2)
  (:method (layer1-symbol layer1 layer2-symbol layer2)))

(defgeneric on-collision-exit (layer1-symbol layer1 layer2-symbol layer2)
  (:method (layer1-symbol layer1 layer2-symbol layer2)))

(defgeneric on-collision-picked (layer-symbol entity)
  (:method (layer-symbol entity)))

(defmacro define-collision-hook (hook layer-specs &body body)
  (u:with-gensyms (layer1-symbol layer2-symbol)
    (case hook
      (:picked
       (destructuring-bind (owner layer) layer-specs
         `(defmethod on-collision-picked ((,layer1-symbol (eql ',layer))
                                          ,owner)
            ,@body)))
      ((:enter :continue :exit)
       (destructuring-bind ((owner1 layer1) (owner2 layer2)) layer-specs
         `(defmethod ,(u:format-symbol :ndjinn "ON-COLLISION-~a" hook)
              ((,layer1-symbol (eql ',layer1)) ,owner1
               (,layer2-symbol (eql ',layer2)) ,owner2)
            (when (and ,owner1 ,owner2)
              ,@body))))
      (t `(error "Invalid hook type: ~s." ',hook)))))
