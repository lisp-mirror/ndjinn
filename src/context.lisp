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

(defun find-context (name)
  (u:if-let ((class-name (u:href =meta/contexts= name)))
    (find-class class-name nil)
    (error "Context ~s is not defined." name)))

(defun make-context (context-name)
  (let ((context (find-context context-name)))
    (make-instance context)))

(defmacro define-context (name () &body body)
  (u:with-gensyms (context-name)
    `(u:eval-always
       (define-config ,name () ,@body)
       (defclass ,context-name (context) ()
         (:default-initargs
          :name ',name))
       (setf (u:href =meta/contexts= ',name) ',context-name))))

(defgeneric on-context-create (context)
  (:method ((context context)))
  (:method :before ((context context))
    (log:debug :ndjinn "Starting initial scene...")
    (switch-scene (cfg :scene))))

(defgeneric on-context-destroy (context)
  (:method ((context context))))

(defun user-data ()
  (%user-data =context=))

(defun (setf user-data) (value)
  (setf (%user-data =context=) value))
