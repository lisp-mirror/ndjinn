(in-package #:net.mfiano.lisp.pyx)

(define-component geometry ()
  ((%geometry/name :reader geometry/name
                   :initarg :geometry/name
                   :initform nil)
   (%geometry/instances :accessor geometry/instances
                        :initarg :geometry/instances
                        :initform 1)
   (%geometry/geometry :accessor geometry/geometry
                       :initform nil)
   (%geometry/dirty :accessor geometry/dirty
                    :initform t))
  (:type-order :after render))

;;; entity hooks

(define-entity-hook :attach (entity geometry)
  (unless geometry/name
    (error "Geometry component ~s does not have a name specified." entity))
  (setf geometry/geometry (make-geometry geometry/name)))

(define-entity-hook :render (entity geometry)
  (when geometry/dirty
    (draw-geometry geometry/geometry geometry/instances)))

(define-entity-hook :delete (entity geometry)
  (delete-geometry geometry/geometry))
