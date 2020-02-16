(in-package #:pyx.component)

(pyx:define-component geometry ()
  ((%geometry/name :reader geometry/name
                   :initarg :geometry/name
                   :initform nil)
   (%geometry/instances :reader geometry/instances
                        :initarg :geometry/instances
                        :initform 1)
   (%geometry/geometry :accessor geometry/geometry
                       :initform nil))
  (:sorting :after render))

;;; entity hooks

(pyx:define-entity-hook :attach (entity geometry)
  (unless geometry/name
    (error "Geometry component ~s does not have a name specified." entity))
  (setf geometry/geometry (pyx::make-geometry geometry/name)))

(pyx:define-entity-hook :render (entity geometry)
  (pyx::draw-geometry geometry/geometry geometry/instances))

(pyx:define-entity-hook :delete (entity geometry)
  (pyx::delete-geometry geometry/geometry))
