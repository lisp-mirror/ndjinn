(in-package #:pyx)

(define-component xform (:after node)
  (:translation (make-translate-state)
   :rotation (make-rotate-state)
   :scaling (make-scale-state)
   :local (m4:id)
   :model (m4:id)))

(defun initialize-translation (entity current incremental)
  (with-slots (%current %previous %incremental) (xform/translation entity)
    (setf %current current
          %previous (v3:copy %current)
          %incremental incremental)))

(defun initialize-rotation (entity current incremental)
  (with-slots (%current %previous %incremental) (xform/rotation entity)
    (setf %current (etypecase current
                     (v3:vec (q:rotate-euler q:+id+ current))
                     (q:quat current))
          %previous (q:copy %current)
          %incremental (etypecase incremental
                         (v3:vec (q:rotate-euler q:+id+ incremental))
                         (q:quat incremental)))))

(defun initialize-scale (entity current incremental)
  (with-slots (%current %previous %incremental) (xform/scaling entity)
    (setf %current (etypecase current
                     (v3:vec current)
                     (real (v3:vec current current current)))
          %previous (v3:copy %current)
          %incremental incremental)))

(defmethod initialize-instance :after ((instance xform)
                                       &key
                                         (xform/translate (v3:zero))
                                         (xform/translate/inc (v3:zero))
                                         (xform/rotate (q:id))
                                         (xform/rotate/inc (q:id))
                                         (xform/scale (v3:one))
                                         (xform/scale/inc (v3:zero)))
  (initialize-translation instance xform/translate xform/translate/inc)
  (initialize-rotation instance xform/rotate xform/rotate/inc)
  (initialize-scale instance xform/scale xform/scale/inc))

(defun transform-node (entity)
  (with-slots (%xform/translation %xform/rotation %xform/scaling) entity
    (let* ((clock (clock *state*))
           (delta (clock-delta-time clock))
           (frame-time (float (clock-frame-time clock) 1f0)))
      (transform-node/vector %xform/scaling delta frame-time)
      (transform-node/quaternion %xform/rotation delta frame-time)
      (transform-node/vector %xform/translation delta frame-time))))

(defun resolve-local (entity factor)
  (with-slots (%xform/translation %xform/rotation %xform/scaling %xform/local)
      entity
    (interpolate-vector %xform/scaling factor)
    (interpolate-quaternion %xform/rotation factor)
    (interpolate-vector %xform/translation factor)
    (m4:copy! %xform/local (q:to-mat4 (interpolated %xform/rotation)))
    (m4:*! %xform/local
           %xform/local
           (m4:set-scale m4:+id+ (interpolated %xform/scaling)))
    (m4:set-translation! %xform/local
                         %xform/local
                         (interpolated %xform/translation))))

(defun resolve-model (entity)
  (with-slots (%node/parent %xform/local %xform/model) entity
    (when %node/parent
      (resolve-local entity (clock-interpolation-factor (clock *state*)))
      (m4:*! %xform/model (xform/model %node/parent) %xform/local))))

(defmethod on-update progn ((entity xform))
  (transform-node entity))

(defmethod on-render progn ((entity xform))
  (with-slots (%xform/model) entity
    (resolve-model entity)
    (when (has-component-p 'render entity)
      (set-uniforms entity :model %xform/model))))
