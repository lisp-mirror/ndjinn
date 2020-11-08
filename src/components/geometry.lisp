(in-package #:net.mfiano.lisp.pyx)

(define-component geometry ()
  ((%geometry/name :reader geometry/name
                   :initarg :geometry/name
                   :initform nil)
   (%geometry/instances :accessor geometry/instances
                        :initarg :geometry/instances
                        :initform 1)
   (%geometry/cache :reader geometry/cache
                    :initarg :geometry/cache
                    :initform t)
   (%geometry/geometry :accessor geometry/geometry
                       :initform nil)
   (%geometry/data :accessor geometry/data
                   :initform (u:dict #'eq))
   (%geometry/dirty :accessor geometry/dirty
                    :initform nil))
  (:type-order :after render))

(defun update-geometry (entity buffer-name data &key replace)
  (cond
    ((has-component-p entity 'geometry)
     (if replace
         (setf (u:href (geometry/data entity) buffer-name) data)
         (push data (u:href (geometry/data entity) buffer-name)))
     (setf (geometry/dirty entity) t))
    (t
     (error "Entity does not have a geometry component attached to update: ~s."
            entity))))

;;; entity hooks

(define-entity-hook :attach (entity geometry)
  (let ((name (geometry/name entity)))
    (unless name
      (error "Geometry component ~s does not have a name specified." entity))
    (let ((geometry (if (geometry/cache entity)
                        (with-asset-cache :geometry name
                            (prog1 (make-geometry name)
                              (log:debug :pyx "Cached geometry: ~a" name)))
                        (make-geometry name))))
      (setf (geometry/geometry entity) geometry))))

(define-entity-hook :pre-render (entity geometry)
  (when (geometry/dirty entity)
    (u:do-hash (k v (geometry/data entity))
      (%update-geometry (geometry/geometry entity) k v))
    (clrhash (geometry/data entity))
    (setf (geometry/dirty entity) nil)))

(define-entity-hook :render (entity geometry)
  (draw-geometry (geometry/geometry entity) (geometry/instances entity)))

(define-entity-hook :delete (entity geometry)
  (u:when-let ((geometry (geometry/geometry entity)))
    (unless (geometry/cache entity)
      (delete-geometry geometry))))
