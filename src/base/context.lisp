(in-package #:net.mfiano.lisp.pyx)

(glob:define-global-var =context= nil)

(defclass context ()
  ((%name :reader name
          :initarg :name)
   (%project :reader project
             :initarg :project)
   (%clock :accessor clock)
   (%initial-scene :reader initial-scene
                   :initarg :initial-scene)
   (%current-scene :accessor current-scene
                   :initform nil)
   (%scenes :reader scenes
            :initform (u:dict #'eq))
   (%framebuffers :reader framebuffers
                  :initform (u:dict #'eq))
   (%display :accessor display
             :initform nil)
   (%input-data :accessor input-data
                :initform nil)
   (%assets :reader assets
            :initform (u:dict #'eq))
   (%shaders :accessor shaders)
   (%running :accessor running
             :initform t)
   (%user-data :accessor %user-data
               :initform nil)
   (%hardware-info :accessor hardware-info
                   :initform nil)
   (%thread-pool :accessor thread-pool
                 :initform nil)
   (%flows :accessor flows
           :initform nil)))

(defun make-context (context-name)
  (if (find-class context-name nil)
      (make-instance context-name)
      (error "Context ~s not defined." context-name)))

(defmacro define-context (name () &body body)
  (destructuring-bind (&key project scene) (car body)
    `(u:eval-always
       (defclass ,name (context) ()
         (:default-initargs
          :name ',name
          :project ,project
          :initial-scene ',scene)))))

(defgeneric on-context-create (context &rest user-args)
  (:method (context &rest user-args)
    (declare (ignore user-args)))
  (:method :before (context &rest user-args)
    (declare (ignore user-args))
    (let ((scene (initial-scene context)))
      (log:debug :pyx "Starting initial scene...")
      (switch-scene scene)
      (log:debug :pyx "Scene loaded: ~s" scene))))

(defgeneric on-context-destroy (context)
  (:method (context)))

(defun user-data ()
  (%user-data =context=))

(defun (setf user-data) (value)
  (setf (%user-data =context=) value))
