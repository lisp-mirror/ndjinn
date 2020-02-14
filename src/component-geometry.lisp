(in-package #:%pyx.component.geometry)

(ent:define-component geometry ()
  ((%name :reader name
          :initarg :geometry/name
          :initform nil)
   (%instances :reader instances
               :initarg :geometry/instances
               :initform 1)
   (%geometry :accessor geometry
              :initform nil))
  (:sorting :after c/render:render))

;;; entity hooks

(ent:define-entity-hook :attach (entity geometry)
  (unless name
    (error "Geometry component ~s does not have a name specified." entity))
  (setf geometry (geom:make-geometry name)))

(ent:define-entity-hook :render (entity geometry)
  (geom:draw-geometry geometry instances))

(ent:define-entity-hook :delete (entity geometry)
  (geom:delete-geometry geometry))
