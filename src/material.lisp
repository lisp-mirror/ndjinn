(in-package #:ndjinn)

;;; spec

(defstruct (material-spec
            (:constructor %make-material-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (master nil :type symbol)
  (slaves nil :type list)
  (shader nil :type symbol)
  (uniforms (make-nested-dict #'eq :self :resolved) :type hash-table)
  (pass nil :type symbol)
  (framebuffer nil :type symbol)
  (attachments nil :type list)
  (render-func (constantly nil) :type function))

(u:define-printer (material-spec stream)
  (format stream "~s" (material-spec-name material-spec)))

(defun find-material-spec (name)
  (or (u:href =meta/materials= name)
      (error "Material ~s is not defined." name)))

(defun find-material-spec-master (spec)
  (let* ((master-name (material-spec-master spec))
         (master-spec (u:href =meta/materials= master-name)))
    (when (and master-name (not master-spec))
      (error "Material ~s inherits from the unknown master ~s."
             (material-spec-name spec)
             master-name))
    master-spec))

(defun copy-material-spec-uniforms (spec)
  (let ((uniforms (u:dict #'eq)))
    (labels ((copy (value)
               (typecase value
                 (sequence (map-into (copy-seq value) #'copy value))
                 (t value))))
      (when spec
        (u:do-hash (k v (u:href (material-spec-uniforms spec) :resolved))
          (setf (u:href uniforms k) (copy v))))
      uniforms)))

(defun update-material-spec-uniforms (spec uniform-data)
  (let* ((uniforms (material-spec-uniforms spec))
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
  (u:when-let ((master (u:href =meta/materials= (material-spec-master spec))))
    (pushnew (material-spec-name spec)
             (material-spec-slaves master))))

(defun update-material-spec-framebuffer-link (material-name framebuffer-name)
  (u:do-hash-values (v =meta/framebuffers=)
    (dolist (framebuffer-material-name (framebuffer-spec-materials v))
      (when (eq material-name framebuffer-material-name)
        (u:deletef (framebuffer-spec-materials v) framebuffer-material-name))))
  (when framebuffer-name
    (push material-name
          (framebuffer-spec-materials (find-framebuffer-spec framebuffer-name)))
    (enqueue :recompile (list :framebuffer framebuffer-name))))

(defun update-material-slave-specs (master-spec)
  (dolist (slave-name (material-spec-slaves master-spec))
    (let ((slave (find-material-spec slave-name)))
      (update-material-spec
       (material-spec-name slave)
       (material-spec-name master-spec)
       (or (material-spec-shader slave)
           (material-spec-shader master-spec))
       (u:hash->plist (u:href (material-spec-uniforms slave) :self))
       (or (material-spec-pass slave)
           (material-spec-pass master-spec))
       (list (or (material-spec-framebuffer slave)
                 (material-spec-framebuffer master-spec))
             (or (material-spec-attachments slave)
                 (material-spec-attachments master-spec)))
       (material-spec-render-func slave)))))

(defun update-material-spec (name master shader uniforms pass output func)
  (let* ((spec (find-material-spec name))
         (master-spec (u:href =meta/materials= master))
         (shader (or shader (and master-spec
                                 (material-spec-shader master-spec))))
         (shader-definition (shadow:find-shader-definition shader)))
    (when (and shader (not shader-definition))
      (error "Shader program ~s is not found for material ~s." shader name))
    (destructuring-bind (&optional framebuffer attachments) output
      (setf (material-spec-master spec) master
            (material-spec-shader spec) shader
            (material-spec-pass spec) (or pass 'default)
            (material-spec-framebuffer spec) framebuffer
            (material-spec-attachments spec) attachments
            (material-spec-render-func spec) func)
      (update-material-spec-framebuffer-link name framebuffer)
      (update-material-spec-uniforms spec uniforms)
      (update-material-spec-relationships spec)
      (enqueue :recompile (list :material name))
      (update-material-slave-specs spec))))

(defun make-material-spec (name master shader uniforms pass output func)
  (let ((spec (%make-material-spec :name name)))
    (setf (u:href =meta/materials= name) spec)
    (update-material-spec name master shader uniforms pass output func)
    spec))

(defmacro define-material (name (&optional master) &body body)
  (destructuring-bind (&key shader uniforms features pass output) (car body)
    (u:with-gensyms (func)
      `(let ((,func ,(generate-render-func features)))
         (if (u:href =meta/materials= ',name)
             (update-material-spec ',name ',master ',shader (list ,@uniforms)
                                   ',pass ',output ,func)
             (make-material-spec ',name ',master ',shader (list ,@uniforms)
                                 ',pass ',output ,func))))))

;;; implementation

(defstruct (material
            (:constructor %make-material)
            (:predicate nil)
            (:copier nil))
  (spec (%make-material-spec) :type material-spec)
  (uniforms (u:dict #'eq) :type hash-table)
  (framebuffer nil :type (or framebuffer null))
  (attachments nil :type list)
  (texture-unit-state 0 :type u:ub8)
  (textures nil :type list))

(u:define-printer (material stream :type t :identity t)
  (format stream "~s" (material-spec-name (material-spec material))))

(defun ensure-material-framebuffer (material)
  (u:when-let* ((spec (material-spec material))
                (framebuffer-name (material-spec-framebuffer spec)))
    (u:if-let ((framebuffer (load-framebuffer framebuffer-name)))
      (let ((attachments (framebuffer-attachment-names->points
                          framebuffer
                          (material-spec-attachments spec))))
        (setf (material-framebuffer material) framebuffer
              (material-attachments material) attachments))
      (error "Material ~s uses unknown framebuffer ~s."
             (material-spec-name spec)
             framebuffer-name))))

(defun make-material-uniforms (material)
  (let ((spec (material-spec material)))
    (clrhash (material-uniforms material))
    (dolist (texture-name (material-textures material))
      (let ((texture (find-asset :texture texture-name)))
        (u:deletef (texture-materials texture) (material-spec-name spec))))
    (setf (material-textures material) nil)
    (u:do-hash (k v (copy-material-spec-uniforms spec))
      (let ((uniform (make-uniform :key k :value v)))
        (register-uniform-func material uniform)
        (load-uniform-texture material uniform)
        (setf (u:href (material-uniforms material) k) uniform)))))

(defun make-material (name)
  (let ((scene-materials (scene-materials (current-scene =context=))))
    (u:if-let ((material (u:href scene-materials name)))
      material
      (let* ((spec (find-material-spec name))
             (material (%make-material :spec spec)))
        (make-material-uniforms material)
        (ensure-material-framebuffer material)
        (setf (u:href scene-materials name) material)
        material))))

(on-recompile :material data ()
  (u:when-let ((shader (material-spec-shader (find-material-spec data))))
    (recompile :shaders (list shader)))
  (u:when-let* ((scene (current-scene =context=))
                (material (u:href (scene-materials scene) data)))
    (make-material-uniforms material)
    (ensure-material-framebuffer material))
  (log:debug :ndjinn "Recompiled material: ~s" data))
