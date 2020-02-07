(in-package #:pyx.examples)

(pyx:define-component world ()
  ((%width :reader width
           :initarg :world/width
           :initform 149)
   (%height :reader height
            :initarg :world/height
            :initform 149)
   (%seed :reader seed
          :initarg :world/seed
          :initform (dungen:make-seed))
   (%density :reader density
             :initarg :world/density
             :initform 0.5)
   (%room-extent :reader room-extent
                 :initarg :world/room-extent
                 :initform 11)
   (%wild-factor :reader wild-factor
                 :initarg :world/wild-factor
                 :initform 0.25)
   (%door-rate :reader door-rate
               :initarg :world/door-rate
               :initform 0.5)
   (%cycle-factor :reader cycle-factor
                  :initarg :world/cycle-factor
                  :initform 0.5)
   (%cell-counts :reader cell-counts
                 :initform (u:dict #'eq))
   (%buffer-name :accessor buffer-name))
  (:sorting :before mesh :after render))

(pyx:define-entity-query-types world
  (:cell-count (:tiles/wall :tiles/floor :tiles/door-v :tiles/door-h)))

(pyx:define-entity-query (entity world) (parameter :cell-count)
  (u:href (cell-counts entity) parameter))

(defun analyze-world (world data)
  (with-slots (%cell-counts) world
    (with-accessors ((width dungen:stage-width)
                     (height dungen:stage-height)
                     (grid dungen:stage-grid))
        data
      (let (walls floors doors/v doors/h)
        (dotimes (x width)
          (dotimes (y height)
            (let* ((cell (aref grid x y))
                   (coords (vector x y)))
              (push coords floors)
              (cond
                ((dungen:has-feature-p cell :wall)
                 (push coords walls))
                ((dungen:has-feature-p cell :door/vertical)
                 (push coords doors/v))
                ((dungen:has-feature-p cell :door/horizontal)
                 (push coords doors/h))))))
        (setf (u:href %cell-counts :tiles/wall) (length walls)
              (u:href %cell-counts :tiles/floor) (length floors)
              (u:href %cell-counts :tiles/door/v) (length doors/v)
              (u:href %cell-counts :tiles/door/h) (length doors/h))
        (values walls floors doors/v doors/h)))))

(defmethod pyx:update-shader-buffer ((object world))
  (with-slots (%width %height %buffer-name) object
    (destructuring-bind (type name) %buffer-name
      (declare (ignore type))
      (u:mvlet* ((data (apply #'dungen:make-stage name))
                 (walls floors doors/v doors/h (analyze-world object data)))
        (pyx:write-shader-buffer %buffer-name :width (list %width))
        (pyx:write-shader-buffer %buffer-name :height (list %height))
        (pyx:write-shader-buffer %buffer-name :cells/floor floors)
        (pyx:write-shader-buffer %buffer-name :cells/wall walls)
        (pyx:write-shader-buffer %buffer-name :cells/door/v doors/v)
        (pyx:write-shader-buffer %buffer-name :cells/door/h doors/h)))))

(pyx:define-entity-hook :attach (entity world)
  (setf buffer-name `(world (:width ,width
                             :height ,height
                             :seed ,seed
                             :density ,density
                             :room-extent ,room-extent
                             :wild-factor ,wild-factor
                             :door-rate ,door-rate
                             :cycle-factor ,cycle-factor)))
  (pyx:with-asset-cache 'world buffer-name
    (pyx:make-shader-buffer buffer-name :world 'shader:world))
  (pyx:update-shader-buffer entity))

(defmethod pyx:delete-asset ((type (eql 'world)) key)
  (pyx:delete-shader-buffer key))

(pyx:define-entity-hook :detach (entity world)
  (pyx:delete-asset 'world buffer-name))
