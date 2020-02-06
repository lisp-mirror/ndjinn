(in-package #:%pyx.context)

(defvar *context* nil)

(defstruct (context (:predicate nil)
                    (:copier nil))
  clock
  current-scene
  (scenes (u:dict #'eq))
  (framebuffers (u:dict #'eq))
  display
  input-data
  (assets (u:dict #'eq))
  shaders
  (running t))

(defun clock ()
  (context-clock *context*))

(defun (setf clock) (value)
  (setf (context-clock *context*) value))

(defun current-scene ()
  (context-current-scene *context*))

(defun (setf current-scene) (value)
  (setf (context-current-scene *context*) value))

(defun scenes ()
  (context-scenes *context*))

(defun framebuffers ()
  (context-framebuffers *context*))

(defun display ()
  (context-display *context*))

(defun (setf display) (value)
  (setf (context-display *context*) value))

(defun input-data ()
  (context-input-data *context*))

(defun (setf input-data) (value)
  (setf (context-input-data *context*) value))

(defun assets ()
  (context-assets *context*))

(defun shaders ()
  (context-shaders *context*))

(defun (setf shaders) (value)
  (setf (context-shaders *context*) value))

(defun running-p ()
  (context-running *context*))

(defun (setf running-p) (value)
  (setf (context-running *context*) value))
