(in-package #:pyx)

(defvar *metadata* (u:dict))

(defun meta (&rest keys)
  (if keys
      (apply #'u:href *metadata* keys)
      *metadata*))

(defun (setf meta) (value &rest keys)
  (setf (apply #'u:href *metadata* keys) value))
