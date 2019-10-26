(in-package #:pyx)

(defclass database ()
  ((%framebuffers :reader framebuffers
                  :initform (u:dict #'eq))
   (%materials :reader materials
               :initform (u:dict #'eq))
   (%picking-ids :reader picking-id
                 :initform (u:dict #'eq))
   (%prefabs :reader prefabs
             :initform (u:dict #'eq))
   (%released-picking-ids :accessor released-picking-ids
                          :initform nil)
   (%shaders :reader shaders)
   (%shader-buffer-bindings :reader shader-buffer-bindings
                            :initform 0)
   (%uuids :reader uuids
           :initform (u:dict #'eq))))

(defun make-database ()
  (setf (slot-value *state* '%database) (make-instance 'database)))
