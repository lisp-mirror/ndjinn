(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-component world ()
  ((%world/width :reader world/width
                 :initarg :world/width
                 :initform 149)
   (%world/height :reader world/height
                  :initarg :world/height
                  :initform 149)
   (%world/seed :reader world/seed
                :initarg :world/seed
                :initform (dun:make-seed))
   (%world/density :reader world/density
                   :initarg :world/density
                   :initform 0.5)
   (%world/room-extent :reader world/room-extent
                       :initarg :world/room-extent
                       :initform 11)
   (%world/wild-factor :reader world/wild-factor
                       :initarg :world/wild-factor
                       :initform 0.25)
   (%world/door-rate :reader world/door-rate
                     :initarg :world/door-rate
                     :initform 0.5)
   (%world/cycle-factor :reader world/cycle-factor
                        :initarg :world/cycle-factor
                        :initform 0.5)
   (%world/cell-counts :reader world/cell-counts
                       :initform (u:dict #'eq))
   (%world/buffer-name :accessor world/buffer-name))
  (:type-order :before pyx:mesh :after pyx:render))

(pyx:define-entity-query-types world
  (:cell-count (:tiles/wall :tiles/floor :tiles/door-v :tiles/door-h)))

(pyx:define-entity-query (entity world) (parameter :cell-count)
  (u:href (world/cell-counts entity) parameter))

(defun analyze-world (world data)
  (let ((cell-counts (world/cell-counts world)))
    (with-accessors ((width dun:stage-width)
                     (height dun:stage-height)
                     (grid dun:stage-grid))
        data
      (let (walls floors doors/v doors/h)
        (dotimes (x width)
          (dotimes (y height)
            (let* ((cell (aref grid x y))
                   (coords (vector x y)))
              (push coords floors)
              (cond
                ((dun:has-feature-p cell :wall)
                 (push coords walls))
                ((dun:has-feature-p cell :door/vertical)
                 (push coords doors/v))
                ((dun:has-feature-p cell :door/horizontal)
                 (push coords doors/h))))))
        (setf (u:href cell-counts :tiles/wall) (length walls)
              (u:href cell-counts :tiles/floor) (length floors)
              (u:href cell-counts :tiles/door/v) (length doors/v)
              (u:href cell-counts :tiles/door/h) (length doors/h))
        (values walls floors doors/v doors/h)))))

(defmethod pyx:update-shader-buffer ((object world))
  (let ((buffer-name (world/buffer-name object)))
    (destructuring-bind (type name) buffer-name
      (declare (ignore type))
      (u:mvlet* ((width (world/width object))
                 (height (world/height object))
                 (data (apply #'dun:make-stage name))
                 (walls floors doors/v doors/h (analyze-world object data)))
        (pyx:write-shader-buffer buffer-name :width (list width))
        (pyx:write-shader-buffer buffer-name :height (list height))
        (pyx:write-shader-buffer buffer-name :cells/floor floors)
        (pyx:write-shader-buffer buffer-name :cells/wall walls)
        (pyx:write-shader-buffer buffer-name :cells/door/v doors/v)
        (pyx:write-shader-buffer buffer-name :cells/door/h doors/h)))))

(pyx:define-entity-hook :attach (entity world)
  (let ((buffer-name `(world (:width ,(world/width entity)
                              :height ,(world/height entity)
                              :seed ,(world/seed entity)
                              :density ,(world/density entity)
                              :room-extent ,(world/room-extent entity)
                              :wild-factor ,(world/wild-factor entity)
                              :door-rate ,(world/door-rate entity)
                              :cycle-factor ,(world/cycle-factor entity)))))
    (setf (world/buffer-name entity) buffer-name)
    (pyx:with-asset-cache 'world buffer-name
      (pyx:make-shader-buffer buffer-name :world 'shader:world))
    (pyx:update-shader-buffer entity)))

(pyx:define-entity-hook :detach (entity world)
  (let ((buffer-name (world/buffer-name entity)))
    (pyx:delete-asset 'world buffer-name)
    (pyx:delete-shader-buffer buffer-name)))
