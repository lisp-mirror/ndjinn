(in-package #:pyx)

(defclass material-spec ()
  ((%id :reader id
        :initarg :id)
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

(defun make-material-spec-uniforms (master-id uniforms)
  (let* ((master (meta :materials master-id))
         (uniforms (or uniforms (u:dict #'eq)))
         (resolved (u:hash-merge (copy-material-spec-uniforms master)
                                 uniforms)))
    (u:dict #'eq
            :self uniforms
            :resolved resolved)))

(defun update-material-spec-relationships (spec)
  (a:when-let ((master (meta :materials (master spec))))
    (pushnew (id spec) (slaves master))))

(defun make-material-spec (&key id shader uniforms master)
  (let* ((uniforms (make-material-spec-uniforms master uniforms))
         (spec (make-instance 'material-spec
                              :id id
                              :shader shader
                              :uniforms uniforms
                              :master master)))
    (update-material-spec-relationships spec)
    spec))

(defun update-material-spec (spec &key shader uniforms master)
  (let ((uniforms (make-material-spec-uniforms master uniforms)))
    (reinitialize-instance spec
                           :shader shader
                           :uniforms uniforms
                           :master master)
    (push-queue :recompile (list :material (id spec)))
    (update-material-spec-relationships spec)
    (dolist (slave-id (slaves spec))
      (let ((slave (meta :materials slave-id)))
        (update-material-spec slave
                              :shader (or shader (shader slave))
                              :uniforms (u:href (uniforms slave) :self)
                              :master (id spec))))
    spec))

(defmacro define-material (id (&optional master) &body body)
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
           (a:if-let ((,spec (meta :materials ',id)))
             (update-material-spec ,spec
                                   :shader ,resolved-shader
                                   :uniforms ,uniforms-table
                                   :master ',master)
             (setf (meta :materials ',id)
                   (make-material-spec :id ',id
                                       :shader ,resolved-shader
                                       :uniforms ,uniforms-table
                                       :master ',master))))))))
