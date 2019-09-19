(in-package #:pyx)

(define-component xform
    (:translate (make-translate-state)
     :rotate (make-rotate-state)
     :scale (make-scale-state)
     :local (m4:id)
     :model (m4:id)))

(defun initialize-translation (entity current incremental)
  (with-slots (%current %previous %incremental) (xform/translate entity)
    (setf %current current
          %previous (v3:copy %current)
          %incremental incremental)))

(defun initialize-rotation (entity current incremental)
  (with-slots (%current %previous %incremental) (xform/rotate entity)
    (setf %current (etypecase current
                     (v3:vec (q:rotate-euler q:+id+ current))
                     (q:quat current))
          %previous (q:copy %current)
          %incremental (etypecase incremental
                         (v3:vec (q:rotate-euler q:+id+ incremental))
                         (q:quat incremental)))))

(defun initialize-scale (entity current incremental)
  (with-slots (%current %previous %incremental) (xform/scale entity)
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
  (with-slots (%xform/translate %xform/rotate %xform/scale) entity
    (let* ((clock (clock *state*))
           (delta (clock-delta-time clock))
           (frame-time (float (clock-frame-time clock) 1f0)))
      (transform-node/vector %xform/scale delta frame-time)
      (transform-node/quaternion %xform/rotate delta frame-time)
      (transform-node/vector %xform/translate delta frame-time))))

(defun resolve-local (entity factor)
  (with-slots (%xform/translate %xform/rotate %xform/scale %xform/local) entity
    (interpolate-vector %xform/scale factor)
    (interpolate-quaternion %xform/rotate factor)
    (interpolate-vector %xform/translate factor)
    (m4:copy! %xform/local (q:to-mat4 (interpolated %xform/rotate)))
    (m4:*! %xform/local
           %xform/local
           (m4:set-scale m4:+id+ (interpolated %xform/scale)))
    (m4:set-translation! %xform/local
                         %xform/local
                         (interpolated %xform/translate))))

(defun resolve-model (entity)
  (with-slots (%node/parent %xform/local %xform/model) entity
    (when %node/parent
      (resolve-local entity (clock-interpolation-factor (clock *state*)))
      (m4:*! %xform/model (xform/model %node/parent) %xform/local))))

(defmethod on-update progn ((entity xform))
  (transform-node entity))

(defmethod on-render progn ((entity xform))
  (with-slots (%xform/model %render/shader) entity
    (resolve-model entity)
    (when (has-component-p 'render entity)
      (with-render %render/shader
        ((:mat4 :model %xform/model))))))
