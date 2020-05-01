(in-package #:pyx)

(defvar *context* nil)

(defclass context ()
  ((%clock :accessor %clock)
   (%current-scene :accessor %current-scene
                   :initform nil)
   (%scenes :reader %scenes
            :initform (u:dict #'eq))
   (%framebuffers :reader %framebuffers
                  :initform (u:dict #'eq))
   (%display :accessor %display)
   (%input-data :accessor %input-data)
   (%assets :reader %assets
            :initform (u:dict #'eq))
   (%shaders :accessor %shaders)
   (%running :accessor %running
             :initform t)
   (%user-data :accessor %user-data
               :initform nil)))

(defun make-context (context-name)
  (if (find-class context-name nil)
      (make-instance context-name)
      (error "Context ~s not defined." context-name)))

(defmacro define-context (name options &body body)
  (declare (ignore options))
  (a:with-gensyms (context-binding)
    (destructuring-bind (&key on-create on-destroy scene options) (car body)
      `(progn
         (defclass ,name (context) ())
         (defmethod get-context-config ((,context-binding ,name))
           ',options)
         (defmethod on-context-create ((,context-binding ,name))
           (switch-scene ',scene)
           ,@(when (fboundp on-create)
               `((funcall ',on-create ,context-binding))))
         (defmethod on-context-destroy ((,context-binding ,name))
           ,@(when (fboundp on-destroy)
               `((funcall ',on-destroy ,context-binding))))))))

(defgeneric on-context-create (context)
  (:method (context)))

(defgeneric on-context-destroy (context)
  (:method (context)))

(defgeneric get-context-config (context)
  (:method (context)))

(defun clock ()
  (%clock *context*))

(defun (setf clock) (value)
  (setf (%clock *context*) value))

(defun current-scene ()
  (%current-scene *context*))

(defun (setf current-scene) (value)
  (setf (%current-scene *context*) value))

(defun scenes ()
  (%scenes *context*))

(defun framebuffers ()
  (%framebuffers *context*))

(defun display ()
  (%display *context*))

(defun (setf display) (value)
  (setf (%display *context*) value))

(defun input-data ()
  (%input-data *context*))

(defun (setf input-data) (value)
  (setf (%input-data *context*) value))

(defun assets ()
  (%assets *context*))

(defun shaders ()
  (%shaders *context*))

(defun (setf shaders) (value)
  (setf (%shaders *context*) value))

(defun running-p ()
  (%running *context*))

(defun (setf running-p) (value)
  (setf (%running *context*) value))

(defun user-data ()
  (%user-data *context*))

(defun (setf user-data) (value)
  (setf (%user-data *context*) value))
