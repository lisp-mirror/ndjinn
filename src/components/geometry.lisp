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
   (%geometry/data :accessor geometry/data
                   :initform nil)
   (%geometry/dirty :accessor geometry/dirty
                    :initform t))
  (:type-order :after render))

;;; entity hooks

(define-entity-hook :attach (entity geometry)
  (let ((name (geometry/name entity)))
    (unless name
      (error "Geometry component ~s does not have a name specified." entity))
    (setf (geometry/geometry entity) (make-geometry name))))

(define-entity-hook :pre-render (entity geometry)
  (u:when-let ((data (geometry/data entity)))
    (update-geometry (geometry/geometry entity) :data data)
    (setf (geometry/data entity) nil)))

(define-entity-hook :render (entity geometry)
  (when (geometry/dirty entity)
    (draw-geometry (geometry/geometry entity) (geometry/instances entity))))

(define-entity-hook :delete (entity geometry)
  (u:when-let ((geometry (geometry/geometry entity)))
    (delete-geometry geometry)))
