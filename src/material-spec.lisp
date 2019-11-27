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
              :initform (make-nested-dict #'eq :self :resolved))
   (%target :reader target
            :initarg :target)))

(defun find-material-spec-master (spec)
  (let* ((master-name (master spec))
         (master-spec (meta :materials master-name)))
    (when (and master-name (not master-spec))
      (error "Material ~s inherits from the unknown master ~s."
             (name spec)
             master-name))
    master-spec))

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

(defun update-material-spec-uniforms (spec uniforms)
  (with-slots (%uniforms) spec
    (let* ((master-spec (find-material-spec-master spec))
           (self (apply #'u:dict #'eq uniforms))
           (resolved (u:hash-merge (copy-material-spec-uniforms master-spec)
                                   self)))
      (clrhash (u:href %uniforms :self))
      (clrhash (u:href %uniforms :resolved))
      (u:do-hash (k v self)
        (setf (u:href %uniforms :self k) v))
      (u:do-hash (k v resolved)
        (setf (u:href %uniforms :resolved k) v)))))

(defun update-material-spec-relationships (spec)
  (a:when-let ((master (meta :materials (master spec))))
    (pushnew (name spec) (slaves master))))

(defun make-material-spec (name master-name shader uniforms target)
  (let ((spec (make-instance 'material-spec :name name
                                            :master master-name
                                            :shader shader
                                            :target target)))
    (update-material-spec-uniforms spec uniforms)
    (update-material-spec-relationships spec)
    spec))

(defun update-material-spec (spec master-name shader uniforms target)
  (with-slots (%name %master %shader %target) spec
    (setf %master master-name
          %shader shader
          %target target)
    (update-material-spec-uniforms spec uniforms)
    (update-material-spec-relationships spec)
    (enqueue :recompile (list :material %name))
    (dolist (slave-name (slaves spec))
      (let ((slave (meta :materials slave-name)))
        (update-material-spec
         slave
         %name
         (or (shader slave) shader)
         (u:hash->plist (u:href (uniforms slave) :self))
         (target slave))))))

(defmacro define-material (name (&optional master) &body body)
  (a:with-gensyms (spec master-spec resolved-shader)
    (destructuring-bind (&key shader target uniforms) (car body)
      `(progn
         (unless (meta :materials)
           (setf (meta :materials) (u:dict #'eq)))
         (let* ((,master-spec (meta :materials ',master))
                (,resolved-shader (or ',shader
                                      (and ,master-spec
                                           (shader ,master-spec)))))

           (a:if-let ((,spec (meta :materials ',name)))
             (update-material-spec ,spec
                                   ',master
                                   ,resolved-shader
                                   (list ,@uniforms)
                                   ',target)
             (setf (meta :materials ',name)
                   (make-material-spec ',name
                                       ',master
                                       ,resolved-shader
                                       (list ,@uniforms)
                                       ',target))))))))
