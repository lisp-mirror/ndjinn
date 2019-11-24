(in-package #:pyx)

(defclass database ()
  ((%framebuffers :reader framebuffers
                  :initform (u:dict #'eq))
   (%materials :reader materials
               :initform (u:dict #'eq))
   (%picking-ids :reader picking-ids
                 :initform (u:dict #'eq))
   (%prefabs :reader prefabs
             :initform (u:dict #'eq))
   (%released-picking-ids :accessor released-picking-ids
                          :initform nil)
   (%shaders :reader shaders)
   (%shader-buffer-bindings :reader shader-buffer-bindings
                            :initform (u:dict #'equalp))
   (%released-shader-buffer-bindings :accessor released-shader-buffer-bindings
                                     :initform nil)
   (%uuids :reader uuids
           :initform (u:dict #'eq))))

(defun make-database ()
  (setf (slot-value *state* '%database) (make-instance 'database)))
