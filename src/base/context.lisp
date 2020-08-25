(in-package #:net.mfiano.lisp.pyx)

(glob:define-global-var =context= nil)

(defclass context ()
  ((%project :reader %project
             :initarg :project)
   (%clock :accessor %clock)
   (%initial-scene :reader %initial-scene
                   :initarg :initial-scene)
   (%current-scene :accessor %current-scene
                   :initform nil)
   (%scenes :reader %scenes
            :initform (u:dict #'eq))
   (%framebuffers :reader %framebuffers
                  :initform (u:dict #'eq))
   (%display :accessor %display
             :initform nil)
   (%input-data :accessor %input-data
                :initform nil)
   (%assets :reader %assets
            :initform (u:dict #'eq))
   (%shaders :accessor %shaders)
   (%running :accessor %running
             :initform t)
   (%end-frame-work :accessor %end-frame-work
                    :initform nil)
   (%user-data :accessor %user-data
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
          :project ,project
          :initial-scene ',scene)))))

(defgeneric on-context-create (context &rest user-args)
  (:method (context &rest user-args)
    (declare (ignore user-args)))
  (:method :before (context &rest user-args)
    (declare (ignore user-args))
    (switch-scene (%initial-scene context))))

(defgeneric on-context-destroy (context)
  (:method (context)))

(defun project ()
  (%project =context=))

(defun clock ()
  (%clock =context=))

(defun (setf clock) (value)
  (setf (%clock =context=) value))

(defun current-scene ()
  (%current-scene =context=))

(defun (setf current-scene) (value)
  (setf (%current-scene =context=) value))

(defun scenes ()
  (%scenes =context=))

(defun framebuffers ()
  (%framebuffers =context=))

(defun display ()
  (%display =context=))

(defun (setf display) (value)
  (setf (%display =context=) value))

(defun input-data ()
  (%input-data =context=))

(defun (setf input-data) (value)
  (setf (%input-data =context=) value))

(defun assets ()
  (%assets =context=))

(defun shaders ()
  (%shaders =context=))

(defun (setf shaders) (value)
  (setf (%shaders =context=) value))

(defun running-p ()
  (%running =context=))

(defun (setf running-p) (value)
  (setf (%running =context=) value))

(defun end-frame-work ()
  (%end-frame-work =context=))

(defun (setf end-frame-work) (value)
  (setf (%end-frame-work =context=) value))

(defun user-data ()
  (%user-data =context=))

(defun (setf user-data) (value)
  (setf (%user-data =context=) value))
