(in-package #:%pyx.component)

(ent:define-component geometry ()
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

(ent:define-entity-hook :attach (entity geometry)
  (unless geometry/name
    (error "Geometry component ~s does not have a name specified." entity))
  (setf geometry/geometry (geom:make-geometry geometry/name)))

(ent:define-entity-hook :render (entity geometry)
  (geom:draw-geometry geometry/geometry geometry/instances))

(ent:define-entity-hook :delete (entity geometry)
  (geom:delete-geometry geometry/geometry))
