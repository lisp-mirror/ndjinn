(in-package #:pyx)

(define-component world (:before mesh :after render)
  (:width 49
   :height 49
   :seed (dungen:make-seed)
   :density 0.5
   :room-extent 11
   :wild-factor 0.25
   :door-rate 0.5
   :cycle-factor 0.5
   :cell-counts (u:dict #'eq)
   :options nil))

(pyx::define-query-types world
  (:cell-count (:tiles/wall :tiles/floor :tiles/door-v :tiles/door-h)))

(pyx::define-query (entity world) (parameter :cell-count)
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

(defmethod update-shader-buffer ((object world) buffer)
  (with-slots (%world/width %world/height %world/options) object
    (u:mvlet* ((data (apply #'dungen:make-stage %world/options))
               (walls floors doors/v doors/h (analyze-world object data)))
      (shadow:write-buffer-path buffer :width (list %world/width))
      (shadow:write-buffer-path buffer :height (list %world/height))
      (shadow:write-buffer-path buffer :cells/floor floors)
      (shadow:write-buffer-path buffer :cells/wall walls)
      (shadow:write-buffer-path buffer :cells/door/v doors/v)
      (shadow:write-buffer-path buffer :cells/door/h doors/h))))

(defmethod shared-initialize :after ((instance world) slot-names &key)
  (with-slots (%world/width %world/height %world/seed %world/density
               %world/room-extent %world/wild-factor %world/door-rate
               %world/cycle-factor %world/options)
      instance
    (setf %world/options `(:width ,%world/width
                           :height ,%world/height
                           :seed ,%world/seed
                           :density ,%world/density
                           :room-extent ,%world/room-extent
                           :wild-factor ,%world/wild-factor
                           :door-rate ,%world/door-rate
                           :cycle-factor ,%world/cycle-factor))
    (resource-lookup :world %world/options
      (make-shader-buffer :world %world/options 'pyx.shader:world)
      (update-shader-buffer instance %world/options))))

(defmethod on-entity-deleted progn ((entity world))
  (delete-shader-buffer (world/options entity)))
