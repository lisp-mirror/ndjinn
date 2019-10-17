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
                     :initarg :component-types
                     :initform (u:dict #'eq :self nil :resolved nil))
   (%component-args :reader component-args
                    :initarg :component-args
                    :initform (u:dict #'eq :self nil :resolved nil))))

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

(defun resolve-prototype-component-types (master-spec types)
  (let ((master-types (when master-spec
                        (u:href (component-types master-spec) :self))))
    (sort (remove-duplicates (append types master-types)) #'string<)))

(defun resolve-prototype-component-args (master-spec args)
  (let ((master-args (when master-spec
                       (u:href (component-args master-spec) :self))))
    (u:alist->plist
     (sort
      (append
       (u:plist->alist args)
       (u:plist->alist
        (apply #'u:plist-remove master-args (u:plist-keys args))))
      #'string<
      :key #'car))))

(defun update-prototype-spec-relationships (spec)
  (a:when-let ((master (meta :prototypes (master spec))))
    (pushnew (name spec) (slaves master))))

(defun update-prototype-spec-tables (spec types args)
  (with-slots (%component-types %component-args) spec
    (let* ((master-spec (find-prototype-spec-master spec))
           (self-types (sort types #'string<))
           (resolved-types (resolve-prototype-component-types
                            master-spec types))
           (self-args (u:alist->plist
                       (sort (u:plist->alist args) #'string< :key #'car)))
           (resolved-args (resolve-prototype-component-args master-spec args)))
      (setf (u:href %component-types :self) self-types
            (u:href %component-types :resolved) resolved-types
            (u:href %component-args :self) self-args
            (u:href %component-args :resolved) resolved-args))))

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
         (u:href (component-types slave) :self)
         (u:href (component-args slave) :self))))))

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
