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
   (%gpu-objects :reader %gpu-objects
                 :initform (u:dict #'eq))))

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

(defun gpu-objects ()
  (%gpu-objects *context*))
