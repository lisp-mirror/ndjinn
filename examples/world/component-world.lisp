(in-package #:pyx.examples)

(pyx:define-component world ()
  ((%world/width :reader world/width
                 :initarg :world/width
                 :initform 149)
   (%world/height :reader world/height
                  :initarg :world/height
                  :initform 149)
   (%world/seed :reader world/seed
                :initarg :world/seed
                :initform (dungen:make-seed))
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
  (:sorting :before mesh :after render))

(pyx:define-query-types world
  (:cell-count (:tiles/wall :tiles/floor :tiles/door-v :tiles/door-h)))

(pyx:define-query (entity world) (parameter :cell-count)
  (u:href (world/cell-counts entity) parameter))

(defun analyze-world (world data)
  (with-slots (%world/cell-counts) world
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
        (setf (u:href %world/cell-counts :tiles/floor) (length floors)
              (u:href %world/cell-counts :tiles/wall) (length walls)
              (u:href %world/cell-counts :tiles/door/v) (length doors/v)
              (u:href %world/cell-counts :tiles/door/h) (length doors/h))
        (values walls floors doors/v doors/h)))))

(defmethod pyx:update-shader-buffer ((object world))
  (with-slots (%world/width %world/height %world/buffer-name) object
    (destructuring-bind (type name) %world/buffer-name
      (declare (ignore type))
      (u:mvlet* ((data (apply #'dungen:make-stage name))
                 (walls floors doors/v doors/h (analyze-world object data)))
        (shadow:write-buffer-path %world/buffer-name :width (list %world/width))
        (shadow:write-buffer-path %world/buffer-name
                                  :height (list %world/height))
        (shadow:write-buffer-path %world/buffer-name :cells/floor floors)
        (shadow:write-buffer-path %world/buffer-name :cells/wall walls)
        (shadow:write-buffer-path %world/buffer-name :cells/door/v doors/v)
        (shadow:write-buffer-path %world/buffer-name :cells/door/h doors/h)))))

;;; entity hooks

(pyx:define-hook :attach (entity world)
  (setf world/buffer-name `(world (:width ,world/width
                                   :height ,world/height
                                   :seed ,world/seed
                                   :density ,world/density
                                   :room-extent ,world/room-extent
                                   :wild-factor ,world/wild-factor
                                   :door-rate ,world/door-rate
                                   :cycle-factor ,world/cycle-factor)))
  (pyx:resource-lookup 'world world/buffer-name
    (pyx:make-shader-buffer world/buffer-name :world 'pyx.examples.shader:world)
    (pyx:update-shader-buffer entity)))

(pyx:define-hook :delete (entity world)
  (pyx:delete-resource 'world world/buffer-name)
  (pyx:delete-shader-buffer world/buffer-name))
