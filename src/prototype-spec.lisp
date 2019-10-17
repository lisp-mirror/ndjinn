(in-package #:pyx)

(defclass prototype-spec ()
  ((%name :reader name
          :initarg :name)
   (%master :accessor master
            :initarg :master
            :initform nil)
   (%slaves :accessor slaves
            :initform nil)
   (%component-types :reader component-types
                     :initform (make-nested-dict #'eq :self :resolved))
   (%component-args :reader component-args
                    :initform (make-nested-dict #'eq :self :resolved))))

(defun find-prototype-spec-master (spec)
  (let* ((master-name (master spec))
         (master-spec (meta :prototypes master-name)))
    (when (and master-name (not master-spec))
      (error "Prototype ~s inherits from the unknown master ~s."
             (name spec)
             master-name))
    master-spec))

(defun generate-prototype-component-args (component-spec)
  (destructuring-bind (type . args) component-spec
    (loop :for (k v) :on args :by #'cddr
          :collect (a:format-symbol :keyword "~a/~a" type k)
          :collect v)))

(defun update-prototype-spec-relationships (spec)
  (a:when-let ((master (meta :prototypes (master spec))))
    (pushnew (name spec) (slaves master))))

(defun update-prototype-spec-tables (spec types args)
  (with-slots (%component-types %component-args) spec
    (let* ((master-spec (find-prototype-spec-master spec))
           (self-types (apply #'u:dict #'eq types))
           (resolved-types (u:hash-merge
                            (if master-spec
                                (u:href (component-types master-spec) :resolved)
                                (u:dict #'eq))
                            self-types))
           (self-args (apply #'u:dict #'eq args))
           (resolved-args (u:hash-merge
                           (if master-spec
                               (u:href (component-args master-spec) :resolved)
                               (u:dict #'eq))
                           self-args)))
      (clrhash (u:href %component-types :self))
      (clrhash (u:href %component-types :resolved))
      (clrhash (u:href %component-args :self))
      (clrhash (u:href %component-args :resolved))
      (u:do-hash (k v self-types)
        (setf (u:href %component-types :self k) v))
      (u:do-hash (k v resolved-types)
        (setf (u:href %component-types :resolved k) v))
      (u:do-hash (k v self-args)
        (setf (u:href %component-args :self k) v))
      (u:do-hash (k v resolved-args)
        (setf (u:href %component-args :resolved k) v)))))

(defun make-prototype-spec (name master-name types args)
  (let ((spec (make-instance 'prototype-spec :name name :master master-name)))
    (update-prototype-spec-tables spec types args)
    (update-prototype-spec-relationships spec)
    spec))

(defun update-prototype-spec (spec master-name types args)
  (with-slots (%name %master) spec
    (setf %master master-name)
    (update-prototype-spec-tables spec types args)
    (enqueue :recompile (list :prototype %name))
    (update-prototype-spec-relationships spec)
    (dolist (slave-name (slaves spec))
      (let ((slave (meta :prototypes slave-name)))
        (update-prototype-spec
         slave
         %name
         (u:hash->plist (u:href (component-types slave) :self))
         (u:hash->plist (u:href (component-args slave) :self)))))))

(defmacro define-prototype (name (&optional master) &body body)
  (a:with-gensyms (spec)
    (let ((types (mapcar #'car body))
          (args `(list ,@(mapcan #'generate-prototype-component-args body))))
      `(progn
         (unless (meta :prototypes)
           (setf (meta :prototypes) (u:dict #'eq)))
         (a:if-let ((,spec (meta :prototypes ',name)))
           (update-prototype-spec ,spec ',master ',types ,args)
           (setf (meta :prototypes ',name)
                 (make-prototype-spec ',name ',master ',types ,args)))))))
