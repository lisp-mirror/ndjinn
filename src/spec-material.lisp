(in-package #:pyx)

(defclass material-spec ()
  ((%name :reader name
          :initarg :name)
   (%master :accessor master
            :initform nil)
   (%slaves :accessor slaves
            :initform nil)
   (%shader :reader shader)
   (%uniforms :reader uniforms
              :initform (make-nested-dict #'eq :self :resolved))
   (%pass :reader pass)
   (%framebuffer :reader framebuffer)
   (%attachments :reader attachments)
   (%render-func :reader render-func
                 :initform (constantly nil))))

(u:define-printer (material-spec stream)
  (format stream "~s" (name material-spec)))

(define-event-handler :recompile :material recompile-material)

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

;;definition
(defun generate-material-render-func (features)
  (destructuring-bind (&key enable disable blend-mode depth-mode
                         polygon-mode line-width point-size)
      features
    (a:with-gensyms (entity material)
      (let ((enable (set-difference enable +gl-capabilities/enabled+))
            (disable (set-difference disable +gl-capabilities/disabled+))
            (blend-mode (and (not (equal blend-mode +gl-blend-mode+))
                             blend-mode))
            (depth-mode (and (not (equal depth-mode +gl-depth-mode+))
                             depth-mode))
            (polygon-mode (and (not (equal polygon-mode +gl-polygon-mode+))
                               polygon-mode)))
        `(lambda (,entity)
           (let ((,material (render/current-material ,entity)))
             (with-framebuffer (framebuffer ,material)
                 (:attachments (attachments ,material))
               (shadow:with-shader (shader (spec ,material))
                 ,@(when enable
                     `((gl:enable ,@enable)))
                 ,@(when disable
                     `((gl:disable ,@disable)))
                 ,@(when blend-mode
                     `((gl:blend-func ,@blend-mode)))
                 ,@(when depth-mode
                     `((gl:depth-func ,depth-mode)))
                 ,@(when polygon-mode
                     `((gl:polygon-mode :front-and-back ,polygon-mode)))
                 ,@(when line-width
                     `((gl:line-width ,line-width)))
                 ,@(when point-size
                     `((gl:point-size ,point-size)))
                 (u:do-hash-values (v (uniforms ,material))
                   (resolve-uniform-func v))
                 (on-entity-render ,entity)
                 (setf (texture-unit-state ,material) 0)
                 ,@(when disable
                     `((gl:enable ,@disable)))
                 ,@(when enable
                     `((gl:disable ,@enable)))
                 ,@(when blend-mode
                     `((gl:blend-func ,@+gl-blend-mode+)))
                 ,@(when depth-mode
                     `((gl:depth-func ,+gl-depth-mode+)))
                 ,@(when polygon-mode
                     `((gl:polygon-mode ,@+gl-polygon-mode+)))
                 ,@(when line-width
                     `((gl:line-width 1f0)))
                 ,@(when point-size
                     `((gl:point-size 1f0)))))))))))

(defun make-material-spec (name master shader uniforms pass output func)
  (let ((spec (make-instance 'material-spec :name name)))
    (setf (meta :materials name) spec)
    (update-material-spec name master shader uniforms  pass output func)
    spec))

(defun update-material-spec (name master shader uniforms pass output func)
  (let ((spec (meta :materials name))
        (master-spec (meta :materials master)))
    (destructuring-bind (&optional framebuffer attachments) output
      (with-slots (%master %shader %uniforms %pass %framebuffer %attachments
                   %render-func)
          spec
        (setf %master master
              %shader (or shader (and master-spec (shader master-spec)))
              %pass (or pass :default)
              %framebuffer framebuffer
              %attachments attachments
              %render-func func)
        (when (and (meta :framebuffers)
                   (meta :framebuffers framebuffer))
          (pushnew name (materials (meta :framebuffers framebuffer))))
        (update-material-spec-uniforms spec uniforms)
        (update-material-spec-relationships spec)
        (enqueue :recompile (list :material name))
        (update-material-slave-specs spec)))))

(defun update-material-slave-specs (master-spec)
  (with-slots (%name %slaves %shader %pass %framebuffer %attachments)
      master-spec
    (dolist (slave-name %slaves)
      (let ((slave (meta :materials slave-name)))
        (update-material-spec
         (name slave)
         %name
         (or (shader slave) %shader)
         (u:hash->plist (u:href (uniforms slave) :self))
         (or (pass slave) %pass)
         (list (or (framebuffer slave) %framebuffer)
               (or (attachments slave) %attachments))
         (render-func slave))))))

(defmacro define-material (name (&optional master) &body body)
  (destructuring-bind (&key shader uniforms features pass output) (car body)
    (a:with-gensyms (func)
      `(let ((,func ,(generate-material-render-func features)))
         (unless (meta :materials)
           (setf (meta :materials) (u:dict #'eq)))
         (if (meta :materials ',name)
             (update-material-spec ',name ',master ',shader (list ,@uniforms)
                                   ',pass ',output ,func)
             (make-material-spec ',name ',master ',shader (list ,@uniforms)
                                 ',pass ',output ,func))))))
