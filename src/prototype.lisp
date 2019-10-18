(in-package #:pyx)

(defclass prototype ()
  ((%name :reader name
          :initarg :name)
   (%master :accessor master
            :initarg :master
            :initform nil)
   (%slaves :accessor slaves
            :initform nil)
   (%component-types :reader component-types
                     :initform (u:dict #'eq :self nil :resolved nil))
   (%component-args :reader component-args
                    :initform (make-nested-dict #'eq :self :resolved))))

(defun find-prototype-master (prototype)
  (let* ((master-name (master prototype))
         (master (meta :prototypes master-name)))
    (when (and master-name (not master))
      (error "Prototype ~s inherits from the unknown master ~s."
             (name prototype)
             master-name))
    master))

(defun generate-prototype-component-args (component-spec)
  (destructuring-bind (type . args) component-spec
    (loop :for (k v) :on args :by #'cddr
          :collect (a:format-symbol :keyword "~a/~a" type k)
          :collect v)))

(defun update-prototype-relationships (prototype)
  (a:when-let ((master (meta :prototypes (master prototype))))
    (pushnew (name prototype) (slaves master))))

(defun update-prototype-tables (prototype types args)
  (with-slots (%component-types %component-args) prototype
    (let* ((master (find-prototype-master prototype))
           (self-types (sort types #'string<))
           (resolved-types (sort
                            (remove-duplicates
                             (append
                              self-types
                              (when master
                                (u:href (component-types master) :resolved))))
                            #'string<))
           (self-args (apply #'u:dict #'eq args))
           (resolved-args (u:hash-merge
                           (if master
                               (u:href (component-args master) :resolved)
                               (u:dict #'eq))
                           self-args)))
      (clrhash (u:href %component-args :self))
      (clrhash (u:href %component-args :resolved))
      (setf (u:href %component-types :self) self-types
            (u:href %component-types :resolved) resolved-types)
      (u:do-hash (k v self-args)
        (setf (u:href %component-args :self k) v))
      (u:do-hash (k v resolved-args)
        (setf (u:href %component-args :resolved k) v)))))

(defun make-prototype (name master-name types args)
  (let ((prototype (make-instance 'prototype :name name :master master-name)))
    (update-prototype-tables prototype types args)
    (update-prototype-relationships prototype)
    prototype))

(defun update-prototype (prototype master-name types args)
  (with-slots (%name %master %slaves) prototype
    (setf %master master-name)
    (update-prototype-tables prototype types args)
    (update-prototype-relationships prototype)
    (dolist (slave-name %slaves)
      (let ((slave (meta :prototypes slave-name)))
        (update-prototype
         slave
         %name
         (u:href (component-types slave) :self)
         (u:hash->plist (u:href (component-args slave) :self)))))))

(defmacro define-prototype (name (&optional master) &body body)
  (a:with-gensyms (prototype)
    (let ((types (mapcar #'car body))
          (args `(list ,@(mapcan #'generate-prototype-component-args body))))
      `(progn
         (unless (meta :prototypes)
           (setf (meta :prototypes) (u:dict #'eq)))
         (a:if-let ((,prototype (meta :prototypes ',name)))
           (update-prototype ,prototype ',master ',types ,args)
           (setf (meta :prototypes ',name)
                 (make-prototype ',name ',master ',types ,args)))))))
