(in-package #:pyx)

(define-component geometry ()
  ((%geometry/name :reader geometry/name
                   :initarg :geometry/name
                   :initform nil)
   (%geometry/spec :accessor geometry/spec
                   :initform nil)
   (%geometry/instances :reader geometry/instances
                        :initarg :geometry/instances
                        :initform 1))
  (:sorting :after render))

;;; entity-hooks

(define-hook :attach (entity geometry)
  (unless geometry/name
    (error "Geometry component must name some geometry."))
  (setf geometry/spec (make-geometry geometry/name)))

(define-hook :render (entity geometry)
  (draw-geometry geometry/spec geometry/instances))
