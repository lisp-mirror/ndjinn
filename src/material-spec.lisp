(in-package #:%pyx.material)

(defstruct (spec (:constructor %make-spec)
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  name
  master
  slaves
  shader
  (spec-uniforms (util:make-nested-dict #'eq :self :resolved))
  pass
  spec-framebuffer
  spec-attachments
  (render-func (constantly nil)))

(u:define-printer (spec stream)
  (format stream "~s" (name spec)))

(defun find-spec (name)
  (or (u:href meta:=materials= name)
      (error "Material ~s is not defined." name)))

(defun find-spec-master (spec)
  (let* ((master-name (master spec))
         (master-spec (u:href meta:=materials= master-name)))
    (when (and master-name (not master-spec))
      (error "Material ~s inherits from the unknown master ~s."
             (name spec)
             master-name))
    master-spec))

(defun copy-spec-uniforms (spec)
  (let ((uniforms (u:dict #'eq)))
    (labels ((copy (value)
               (typecase value
                 (sequence (map-into (copy-seq value) #'copy value))
                 (t value))))
      (when spec
        (u:do-hash (k v (u:href (spec-uniforms spec) :resolved))
          (setf (u:href uniforms k) (copy v))))
      uniforms)))

(defun update-spec-uniforms (spec uniform-data)
  (let* ((uniforms (spec-uniforms spec))
         (master-spec (find-spec-master spec))
         (self (apply #'u:dict #'eq uniform-data))
         (resolved (u:hash-merge (copy-spec-uniforms master-spec) self)))
    (clrhash (u:href uniforms :self))
    (clrhash (u:href uniforms :resolved))
    (u:do-hash (k v self)
      (setf (u:href uniforms :self k) v))
    (u:do-hash (k v resolved)
      (setf (u:href uniforms :resolved k) v))))

(defun update-spec-relationships (spec)
  (a:when-let ((master (u:href meta:=materials= (master spec))))
    (pushnew (name spec) (slaves master))))

(defun update-spec-framebuffer-link (material-name framebuffer-name)
  (u:do-hash-values (v meta:=framebuffers=)
    (dolist (framebuffer-material-name (fb:materials v))
      (when (eq material-name framebuffer-material-name)
        (a:deletef (fb:materials v) framebuffer-material-name))))
  (when framebuffer-name
    (push material-name (fb:materials (fb:find-spec framebuffer-name)))
    (tp:enqueue :recompile (list :framebuffer framebuffer-name))))

(defun update-spec (name master shader uniforms pass output func)
  (let ((spec (find-spec name))
        (master-spec (u:href meta:=materials= master)))
    (destructuring-bind (&optional framebuffer attachments) output
      (setf (master spec) master
            (shader spec) (or shader (and master-spec (shader master-spec)))
            (pass spec) (or pass 'ext:default)
            (spec-framebuffer spec) framebuffer
            (spec-attachments spec) attachments
            (render-func spec) func)
      (update-spec-framebuffer-link name framebuffer)
      (update-spec-uniforms spec uniforms)
      (update-spec-relationships spec)
      (tp:enqueue :recompile (list :material name))
      (update-slave-specs spec))))

(defun make-spec (name master shader uniforms pass output func)
  (let ((spec (%make-spec :name name)))
    (setf (u:href meta:=materials= name) spec)
    (update-spec name master shader uniforms  pass output func)
    spec))

(defun update-slave-specs (master-spec)
  (dolist (slave-name (slaves master-spec))
    (let ((slave (find-spec slave-name)))
      (update-spec
       (name slave)
       (name master-spec)
       (or (shader slave) (shader master-spec))
       (u:hash->plist (u:href (spec-uniforms slave) :self))
       (or (pass slave) (pass master-spec))
       (list (or (spec-framebuffer slave) (spec-framebuffer master-spec))
             (or (spec-attachments slave) (spec-attachments master-spec)))
       (render-func slave)))))

;;; Public API

(defmacro define-material (name (&optional master) &body body)
  (destructuring-bind (&key shader uniforms features pass output) (car body)
    (a:with-gensyms (func)
      `(let ((,func ,(c/render:generate-render-func features)))
         (if (u:href meta:=materials= ',name)
             (update-spec ',name ',master ',shader (list ,@uniforms) ',pass
                          ',output ,func)
             (make-spec ',name ',master ',shader (list ,@uniforms) ',pass
                        ',output ,func))))))
