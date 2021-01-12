(in-package #:ndjinn)

(defclass context ()
  ((%name :reader name
          :initarg :name)
   (%project :reader project
             :initarg :project)
   (%assets :reader assets
            :initform (u:dict #'eq))
   (%clock :accessor clock)
   (%current-scene :accessor current-scene
                   :initform nil)
   (%display :accessor display
             :initform nil)
   (%framebuffers :reader framebuffers
                  :initform (u:dict #'eq))
   (%hardware-info :accessor hardware-info
                   :initform nil)
   (%input-data :accessor input-data
                :initform nil)
   (%running :accessor running
             :initform t)
   (%scenes :reader scenes
            :initform (u:dict #'eq))
   (%shaders :accessor shaders)
   (%user-data :accessor %user-data
               :initform nil)
   (%thread-pool :accessor thread-pool
                 :initform nil)
   (%delayed-work :accessor delayed-work
                  :initform (make-delayed-work))))

(glob:define-global-var =context= nil)

(defun make-context-symbol (name)
  (u:format-symbol (symbol-package name) "CONTEXT/~a" name))

(defun make-context (name)
  (let ((context (make-context-symbol name)))
    (make-instance context)))

(defmacro define-context (name () &body body)
  `(u:eval-always
     (define-config ,name () ,@body)
     (defclass ,(make-context-symbol name) (context) ()
       (:default-initargs
        :name ',name))))

(defmacro define-context-hook (hook (context) &body body)
  (let ((method (u:format-symbol :ndjinn "ON-CONTEXT-~a" hook)))
    `(defmethod ,method ((context ,(make-context-symbol context)))
       ,@body)))

(defgeneric on-context-pre-create (context)
  (:method ((context context))))

(defgeneric on-context-post-create (context)
  (:method ((context context)))
  (:method :before ((context context))
    (log:debug :ndjinn "Starting initial scene...")
    (switch-scene (cfg :scene))))

(defun on-context-create (context)
  (on-context-pre-create context)
  (on-context-post-create context))

(defgeneric on-context-destroy (context)
  (:method ((context context))))

(defun user-data ()
  (%user-data =context=))

(defun (setf user-data) (value)
  (setf (%user-data =context=) value))
