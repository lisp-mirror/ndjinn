(in-package #:pyx)

(define-component collider/cuboid (collider)
  ((%collider/min-x :reader collider/min-x
                    :initarg :collider/min-x
                    :initform -1f0)
   (%collider/max-x :reader collider/max-x
                    :initarg :collider/max-x
                    :initform 1f0)
   (%collider/min-y :reader collider/min-y
                    :initarg :collider/min-y
                    :initform -1f0)
   (%collider/max-y :reader collider/max-y
                    :initarg :collider/max-y
                    :initform 1f0)
   (%collider/min-z :reader collider/min-z
                    :initarg :collider/min-z
                    :initform -1f0)
   (%collider/max-z :reader collider/max-z
                    :initarg :collider/max-z
                    :initform 1f0)
   (%collider/obb :accessor collider/obb
                  :initform nil))
  (:sorting :after render))

(defun scale-collider/cuboid (entity)
  (with-slots (%collider/min-x %collider/max-x %collider/min-y %collider/max-y
               %collider/min-z %collider/max-z)
      entity
    (v3:with-components ((s (current (xform/scaling entity))))
      (v3:*! s s (v3:vec (- %collider/max-x %collider/min-x)
                         (- %collider/max-y %collider/min-y)
                         (- %collider/max-z %collider/min-z))))))

(define-hook :update (entity collider/cuboid)
  (let* ((min (transform-point
               entity
               (v3:+ collider/center
                     (v3:vec collider/min-x collider/min-y collider/min-z))))
         (max (transform-point
               entity
               (v3:+ collider/center
                     (v3:vec collider/max-x collider/max-y collider/max-z))))
         (center (v3:lerp min max 0.5))
         (axes (m4:rotation-to-mat3
                (m4:normalize-rotation
                 (xform/model entity))))
         (diagonal (v3:- max center))
         (half-widths (v3:vec (v3:dot diagonal (m3:get-column axes 0))
                              (v3:dot diagonal (m3:get-column axes 1))
                              (v3:dot diagonal (m3:get-column axes 2)))))
    (setf collider/obb (make-oriented-bounding-box center axes half-widths))))

(define-hook :attach (entity collider/cuboid)
  (scale-collider/cuboid entity)
  (initialize-collider-visualization entity "cuboid")
  (register-collider entity))

(define-hook :detach (entity collider/cuboid)
  (deregister-collider entity)
  (setf collider/target nil))
