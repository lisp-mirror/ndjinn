(in-package #:pyx)

(define-component collider/cuboid (collider)
  ((%collider/min-x :reader collider/min-x
                    :initarg :collider/min-x
                    :initform -0.5f0)
   (%collider/max-x :reader collider/max-x
                    :initarg :collider/max-x
                    :initform 0.5f0)
   (%collider/min-y :reader collider/min-y
                    :initarg :collider/min-y
                    :initform -0.5f0)
   (%collider/max-y :reader collider/max-y
                    :initarg :collider/max-y
                    :initform 0.5f0)
   (%collider/min-z :reader collider/min-z
                    :initarg :collider/min-z
                    :initform -0.5f0)
   (%collider/max-z :reader collider/max-z
                    :initarg :collider/max-z
                    :initform 0.5f0))
  (:sorting :after render))
