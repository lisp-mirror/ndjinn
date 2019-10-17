(in-package #:pyx)

(defclass material-spec ()
  ((%name :reader name
          :initarg :name)
   (%master :accessor master
            :initarg :master
            :initform nil)
   (%slaves :accessor slaves
            :initform nil)
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms)))

(defun copy-material-spec-uniforms (spec)
  (let ((uniforms (u:dict #'eq)))
    (labels ((copy (value)
               (typecase value
                 (sequence (map-into (copy-seq value) #'copy value))
                 (t value))))
      (when spec
        (u:do-hash (k v (u:href (uniforms spec) :resolved))
          (setf (u:href uniforms k) (copy v))))
      uniforms)))

(defun make-material-spec-uniforms (master-name uniforms)
  (let* ((master (meta :materials master-name))
         (uniforms (or uniforms (u:dict #'eq)))
         (resolved (u:hash-merge (copy-material-spec-uniforms master)
                                 uniforms)))
    (u:dict #'eq
            :self uniforms
            :resolved resolved)))

(defun update-material-spec-relationships (spec)
  (a:when-let ((master (meta :materials (master spec))))
    (pushnew (name spec) (slaves master))))

(defun make-material-spec (&key name master shader uniforms)
  (let* ((uniforms (make-material-spec-uniforms master uniforms))
         (spec (make-instance 'material-spec
                              :name name
                              :master master
                              :shader shader
                              :uniforms uniforms)))
    (update-material-spec-relationships spec)
    spec))

(defun update-material-spec (spec &key master shader uniforms)
  (let ((uniforms (make-material-spec-uniforms master uniforms)))
    (reinitialize-instance spec
                           :master master
                           :shader shader
                           :uniforms uniforms)
    (enqueue :recompile (list :material (name spec)))
    (update-material-spec-relationships spec)
    (dolist (slave-name (slaves spec))
      (let ((slave (meta :materials slave-name)))
        (update-material-spec slave
                              :master (name spec)
                              :shader (or (shader slave) shader)
                              :uniforms (u:href (uniforms slave) :self))))
    spec))

(defmacro define-material (name (&optional master) &body body)
  (a:with-gensyms (spec master-spec uniforms-table resolved-shader)
    (destructuring-bind (&key shader uniforms) (car body)
      `(progn
         (unless (meta :materials)
           (setf (meta :materials) (u:dict #'eq)))
         (let* ((,master-spec (meta :materials ',master))
                (,uniforms-table (u:dict #'eq ,@uniforms))
                (,resolved-shader (or ',shader
                                      (and ,master-spec
                                           (shader ,master-spec)))))
           (a:if-let ((,spec (meta :materials ',name)))
             (update-material-spec ,spec
                                   :master ',master
                                   :shader ,resolved-shader
                                   :uniforms ,uniforms-table)
             (setf (meta :materials ',name)
                   (make-material-spec :name ',name
                                       :master ',master
                                       :shader ,resolved-shader
                                       :uniforms ,uniforms-table))))))))
