(in-package #:pyx)

(defclass scene-spec ()
  ((%name :reader name
          :initarg :name)
   (%prefab :reader prefab
            :initarg :prefab)
   (%pipeline :reader pipeline
              :initarg :pipeline)))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key prefab (pipeline '(default))) (car body)
    `(progn
       (unless (meta :scenes)
         (setf (meta :scenes) (u:dict #'eq)))
       (setf (meta :scenes ',name)
             (make-instance 'scene-spec
                            :name ',name
                            :prefab ',prefab
                            :pipeline ',pipeline)))))

;;;

(defclass scene ()
  ((%spec :reader spec
          :initarg :spec)))

(u:define-printer (scene stream)
  (format stream "~s" (name (spec scene))))

(defun load-scene (scene-name)
  (let ((spec (meta :scenes scene-name)))
    (unless spec
      (error "Scene ~s is not defined." scene-name))
    (let ((scene (make-instance 'scene :spec spec)))
      (setf (u:href (scenes *state*) scene-name) scene
            (slot-value *state* '%current-scene) scene)
      (load-prefab (prefab spec)))))
