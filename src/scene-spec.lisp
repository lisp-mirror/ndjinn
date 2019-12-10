(in-package #:pyx)

(defclass scene-spec ()
  ((%name :reader name
          :initarg :name)
   (%prefabs :reader prefabs
             :initarg :prefabs)
   (%pipeline :reader pipeline
              :initarg :pipeline)))

(u:define-printer (scene-spec stream)
  (format stream "~s" (name scene-spec)))

(define-event-handler :recompile :scene recompile-scene)

(defun make-scene-spec (name prefabs pipeline)
  (let ((spec (make-instance 'scene-spec :name name)))
    (setf (meta :scenes name) spec)
    (update-scene-spec name prefabs pipeline)
    spec))

(defun update-scene-spec (name prefabs pipeline)
  (let ((pipeline (find-pipeline-spec pipeline)))
    (with-slots (%prefabs %pipeline) (meta :scenes name)
      (setf %prefabs prefabs
            %pipeline pipeline)
      (pushnew name (scenes pipeline))
      (enqueue :recompile (list :scene name)))))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key prefabs (pipeline :default)) (car body)
    `(progn
       (unless (meta :scenes)
         (setf (meta :scenes) (u:dict #'eq)))
       (if (meta :scenes ',name)
           (update-scene-spec ',name ',prefabs ',pipeline)
           (make-scene-spec ',name ',prefabs ',pipeline)))))
