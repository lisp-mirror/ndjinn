(in-package #:pyx)

(define-component world (:before mesh :after render)
  (:options nil
   :level nil
   :cell-counts (u:dict #'eq)
   :metadata nil))

(defun make-world (level &rest args)
  (let ((world (make-entity (world)
                 :xform/scale 50
                 :world/options args
                 :world/level level)))
    (make-entity (render mesh)
      :node/parent world
      :xform/scale (v3:vec 0.5 0.5 0.1)
      :render/shader 'pyx.shader:world
      :render/uniforms (u:dict
                        :cell-type 0
                        :light.position (v3:vec 0.1 0.25 -1)
                        :light.ambient (v4:vec 0.01 0.01 0.01 0.01)
                        :light.diffuse (v4:vec 0.5 0.5 0.5 0.5)
                        :light.specular (v4:vec 0.2 0.2 0.2 0.2)
                        :material.ambient (v4:one)
                        :material.diffuse (v4:one)
                        :material.specular (v4:one)
                        :material.shininess 10
                        :opacity 1.0)
      :mesh/file "floor.glb"
      :mesh/instances (u:href (world/cell-counts world) :floor))
    (make-entity (render mesh)
      :node/parent world
      :xform/translate (v3:vec 0 0 0.75)
      :xform/scale (v3:vec 0.5 0.5 0.75)
      :render/shader 'pyx.shader:world
      :render/uniforms (u:dict
                        :cell-type 1
                        :light.position (v3:vec 0.1 0.25 -1)
                        :light.ambient (v4:vec 0.01 0.01 0.01 0.01)
                        :light.diffuse (v4:vec 0.5 0.5 0.5 0.5)
                        :light.specular (v4:vec 0.2 0.2 0.2 0.2)
                        :material.ambient (v4:one)
                        :material.diffuse (v4:one)
                        :material.specular (v4:one)
                        :material.shininess 10
                        :opacity 1.0)
      :mesh/file "wall.glb"
      :mesh/instances (u:href (world/cell-counts world) :wall))))

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
        (setf (u:href %world/cell-counts :floor) (length floors)
              (u:href %world/cell-counts :wall) (length walls)
              (u:href %world/cell-counts :door/v) (length doors/v)
              (u:href %world/cell-counts :door/h) (length doors/h))
        (values walls floors doors/v doors/h)))))

(defun write-world-buffer (world data buffer)
  (destructuring-bind (&key width height &allow-other-keys)
      (world/options world)
    (u:mvlet ((walls floors doors/v doors/h (analyze-world world data)))
      (shadow:write-buffer-path buffer :width (list width))
      (shadow:write-buffer-path buffer :height (list height))
      (shadow:write-buffer-path buffer :cells/floor floors)
      (shadow:write-buffer-path buffer :cells/wall walls)
      (shadow:write-buffer-path buffer :cells/door/v doors/v)
      (shadow:write-buffer-path buffer :cells/door/h doors/h))))

(defun make-world-data (world)
  (let ((data (apply #'dungen:make-stage (world/options world))))
    (make-shader-buffer :world 'pyx.shader:world)
    (write-world-buffer world data :world)
    data))

(defmethod shared-initialize :after ((instance world) slot-names &key)
  (with-slots (%world/level %world/metadata) instance
    (setf %world/metadata (cache-lookup :world %world/level
                            (make-world-data instance)))))
