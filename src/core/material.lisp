(in-package #:pyx)

;;; spec

(defclass material-spec ()
  ((%name :reader name
          :initarg :name)
   (%master :accessor master
            :initform nil)
   (%slaves :accessor slaves
            :initform nil)
   (%shader :accessor shader
            :initform nil)
   (%uniforms :reader uniforms
              :initform (make-nested-dict #'eq :self :resolved))
   (%pass :accessor pass
          :initform nil)
   (%framebuffer :accessor framebuffer
                 :initform nil)
   (%attachments :accessor attachments
                 :initform nil)
   (%render-func :accessor render-func
                 :initform (constantly nil))))

(u:define-printer (material-spec stream)
  (format stream "~s" (name material-spec)))

(defun find-material-spec (name)
  (or (u:href =materials= name)
      (error "Material ~s is not defined." name)))

(defun find-material-spec-master (spec)
  (let* ((master-name (master spec))
         (master-spec (u:href =materials= master-name)))
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

(defun update-material-spec-uniforms (spec uniform-data)
  (let* ((uniforms (uniforms spec))
         (master-spec (find-material-spec-master spec))
         (self (apply #'u:dict #'eq uniform-data))
         (resolved (u:hash-merge (copy-material-spec-uniforms master-spec)
                                 self)))
    (clrhash (u:href uniforms :self))
    (clrhash (u:href uniforms :resolved))
    (u:do-hash (k v self)
      (setf (u:href uniforms :self k) v))
    (u:do-hash (k v resolved)
      (setf (u:href uniforms :resolved k) v))))

(defun update-material-spec-relationships (spec)
  (a:when-let ((master (u:href =materials= (master spec))))
    (pushnew (name spec) (slaves master))))

(defun update-material-spec-framebuffer-link (material-name framebuffer-name)
  (u:do-hash-values (v =framebuffers=)
    (dolist (framebuffer-material-name (materials v))
      (when (eq material-name framebuffer-material-name)
        (a:deletef (materials v) framebuffer-material-name))))
  (when framebuffer-name
    (push material-name (materials (find-framebuffer-spec framebuffer-name)))
    (enqueue :recompile (list :framebuffer framebuffer-name))))

(defun update-material-spec (name master shader uniforms pass output func)
  (let ((spec (find-material-spec name))
        (master-spec (u:href =materials= master)))
    (destructuring-bind (&optional framebuffer attachments) output
      (setf (master spec) master
            (shader spec) (or shader (and master-spec (shader master-spec)))
            (pass spec) (or pass 'default)
            (framebuffer spec) framebuffer
            (attachments spec) attachments
            (render-func spec) func)
      (update-material-spec-framebuffer-link name framebuffer)
      (update-material-spec-uniforms spec uniforms)
      (update-material-spec-relationships spec)
      (enqueue :recompile (list :material name))
      (update-material-slave-specs spec))))

(defun make-material-spec (name master shader uniforms pass output func)
  (let ((spec (make-instance 'material-spec :name name)))
    (setf (u:href =materials= name) spec)
    (update-material-spec name master shader uniforms pass output func)
    spec))

(defun update-material-slave-specs (master-spec)
  (dolist (slave-name (slaves master-spec))
    (let ((slave (find-material-spec slave-name)))
      (update-material-spec
       (name slave)
       (name master-spec)
       (or (shader slave) (shader master-spec))
       (u:hash->plist (u:href (uniforms slave) :self))
       (or (pass slave) (pass master-spec))
       (list (or (framebuffer slave) (framebuffer master-spec))
             (or (attachments slave) (attachments master-spec)))
       (render-func slave)))))

(defmacro define-material (name (&optional master) &body body)
  (destructuring-bind (&key shader uniforms features pass output) (car body)
    (a:with-gensyms (func)
      `(let ((,func ,(comp::generate-render-func features)))
         (if (u:href =materials= ',name)
             (update-material-spec ',name ',master ',shader (list ,@uniforms)
                                   ',pass ',output ,func)
             (make-material-spec ',name ',master ',shader (list ,@uniforms)
                                 ',pass ',output ,func))))))

;;; implementation

(defclass material ()
  ((%spec :reader spec
          :initarg :spec)
   (%entity :reader entity
            :initarg :entity)
   (%uniforms :reader uniforms
              :initform (u:dict #'eq))
   (%framebuffer :accessor framebuffer
                 :initform nil)
   (%attachments :accessor attachments
                 :initform nil)
   (%texture-unit-state :accessor texture-unit-state
                        :initform 0)))

(u:define-printer (material stream :type t :identity t)
  (format stream "~s" (name (spec material))))

(defun ensure-material-framebuffer (material)
  (a:when-let* ((spec (spec material))
                (framebuffer-name (framebuffer spec)))
    (a:if-let ((framebuffer (load-framebuffer framebuffer-name)))
      (setf (framebuffer material) framebuffer
            (attachments material) (framebuffer-attachment-names->points
                                    framebuffer
                                    (attachments spec)))
      (error "Material ~s uses unknown framebuffer ~s."
             (name spec)
             framebuffer-name))))

(defun make-material-uniforms (material)
  (clrhash (uniforms material))
  (u:do-hash (k v (copy-material-spec-uniforms (spec material)))
    (let ((uniform (make-uniform :key k :value v)))
      (register-uniform-func material uniform)
      (load-uniform-texture uniform)
      (setf (u:href (uniforms material) k) uniform))))

(defun make-material (entity name)
  (let* ((spec (find-material-spec name))
         (material (make-instance 'material :entity entity :spec spec)))
    (make-material-uniforms material)
    (ensure-material-framebuffer material)
    (push material (u:href (materials (current-scene)) name))
    material))

(on-recompile :material data ()
  (let ((shader (shader (find-material-spec data))))
    (recompile :shaders (list shader))
    (dolist (material (u:href (materials (current-scene)) data))
      (make-material-uniforms material)
      (ensure-material-framebuffer material))))
