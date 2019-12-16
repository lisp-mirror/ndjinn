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
   (%blend-mode :reader blend-mode)
   (%depth-mode :reader depth-mode)
   (%enabled :reader enabled)
   (%disabled :reader disabled)
   (%pass :reader pass)
   (%framebuffer :reader framebuffer)
   (%attachments :reader attachments)))

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

(defun make-material-spec (name master shader uniforms blend-mode depth-mode
                           features pass output)
  (let ((spec (make-instance 'material-spec :name name)))
    (setf (meta :materials name) spec)
    (update-material-spec name master shader uniforms blend-mode depth-mode
                          features pass output)
    spec))

(defun update-material-spec (name master shader uniforms blend-mode depth-mode
                             features pass output)
  (let ((spec (meta :materials name))
        (master-spec (meta :materials master)))
    (destructuring-bind (&optional framebuffer attachments) output
      (destructuring-bind (&key enable disable) features
        (with-slots (%master %shader %uniforms %blend-mode %depth-mode %enabled
                     %disabled %pass %framebuffer %attachments)
            spec
          (setf %master master
                %shader (or shader (and master-spec (shader master-spec)))
                %blend-mode (or blend-mode +gl-blend-mode+)
                %depth-mode (or depth-mode +gl-depth-mode+)
                %enabled (set-difference enable +gl-capabilities/enabled+)
                %disabled (set-difference disable +gl-capabilities/disabled+)
                %pass (or pass :default)
                %framebuffer framebuffer
                %attachments attachments)
          (when (and (meta :framebuffers)
                     (meta :framebuffers framebuffer))
            (pushnew name (materials (meta :framebuffers framebuffer))))
          (update-material-spec-uniforms spec uniforms)
          (update-material-spec-relationships spec)
          (enqueue :recompile (list :material name))
          (update-material-slave-specs spec))))))

(defun update-material-slave-specs (master-spec)
  (with-slots (%name %slaves %shader %blend-mode %depth-mode %enabled %disabled
               %pass %framebuffer %attachments)
      master-spec
    (dolist (slave-name %slaves)
      (let ((slave (meta :materials slave-name)))
        (update-material-spec
         (name slave)
         %name
         (or (shader slave) %shader)
         (u:hash->plist (u:href (uniforms slave) :self))
         (or (blend-mode slave) %blend-mode)
         (or (depth-mode slave) %depth-mode)
         (list (or (enabled slave) %enabled)
               (or (disabled slave) %disabled))
         (or (pass slave) %pass)
         (list (or (framebuffer slave) %framebuffer)
               (or (attachments slave) %attachments)))))))

(defmacro define-material (name (&optional master) &body body)
  (destructuring-bind (&key shader uniforms blend-mode depth-mode features
                         pass output)
      (car body)
    `(progn
       (unless (meta :materials)
         (setf (meta :materials) (u:dict #'eq)))
       (if (meta :materials ',name)
           (update-material-spec ',name ',master ',shader (list ,@uniforms)
                                 ',blend-mode ',depth-mode ',features ',pass
                                 ',output)
           (make-material-spec ',name ',master ',shader (list ,@uniforms)
                               ',blend-mode ',depth-mode ',features ',pass
                               ',output)))))
