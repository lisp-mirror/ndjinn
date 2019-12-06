(in-package #:pyx)

(defclass scene-spec ()
  ((%name :reader name
          :initarg :name)
   (%prefabs :reader prefabs
             :initarg :prefabs)
   (%pipeline :reader pipeline
              :initarg :pipeline)))

(defun update-scene-spec (name prefabs pipeline)
  (with-slots (%prefabs %pipeline) (meta :scenes name)
    (setf %prefabs prefabs
          %pipeline pipeline)
    (enqueue :recompile (list :scene name))))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key prefabs (pipeline '(default))) (car body)
    `(progn
       (unless (meta :scenes)
         (setf (meta :scenes) (u:dict #'eq)))
       (if (meta :scenes ',name)
           (update-scene-spec ',name ',prefabs ',pipeline)
           (setf (meta :scenes ',name)
                 (make-instance 'scene-spec
                                :name ',name
                                :prefabs ',prefabs
                                :pipeline ',pipeline))))))
