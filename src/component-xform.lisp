(in-package #:pyx)

(define-component xform ()
  ((%xform/translation :reader xform/translation
                       :initform (make-translate-state))
   (%xform/rotation :reader xform/rotation
                    :initform (make-rotate-state))
   (%xform/scaling :reader xform/scaling
                   :initform (make-scale-state))
   (%xform/local :reader xform/local
                 :initform (m4:id))
   (%xform/model :reader xform/model
                 :initform (m4:id)))
  (:sorting :after node)
  (:static t))

(defun initialize-translation (entity current)
  (with-slots (%current %previous) (xform/translation entity)
    (setf %current current
          %previous (v3:copy %current))))

(defun initialize-translation/inc (entity incremental)
  (with-slots (%incremental) (xform/translation entity)
    (setf %incremental incremental)))

(defun initialize-rotation (entity current)
  (with-slots (%current %previous) (xform/rotation entity)
    (setf %current (etypecase current
                     (v3:vec (q:rotate-euler q:+id+ current))
                     (q:quat current))
          %previous (q:copy %current))))

(defun initialize-rotation/inc (entity incremental)
  (with-slots (%incremental) (xform/rotation entity)
    (setf %incremental (etypecase incremental
                         (v3:vec (q:rotate-euler q:+id+ incremental))
                         (q:quat incremental)))))

(defun initialize-scaling (entity current)
  (with-slots (%current %previous) (xform/scaling entity)
    (setf %current (etypecase current
                     (v3:vec current)
                     (real (v3:vec current current current)))
          %previous (v3:copy %current))))

(defun initialize-scaling/inc (entity incremental)
  (with-slots (%incremental) (xform/scaling entity)
    (setf %incremental incremental)))

(defmethod shared-initialize :after ((instance xform) slot-names
                                     &key
                                       (xform/translate (v3:zero))
                                       (xform/translate/inc (v3:zero))
                                       (xform/rotate (q:id))
                                       (xform/rotate/inc (q:id))
                                       (xform/scale (v3:one))
                                       (xform/scale/inc (v3:zero)))
  (initialize-translation instance xform/translate)
  (initialize-translation/inc instance xform/translate/inc)
  (initialize-rotation instance xform/rotate)
  (initialize-rotation/inc instance xform/rotate/inc)
  (initialize-scaling instance xform/scale)
  (initialize-scaling/inc instance xform/scale/inc))

(defmethod reinitialize-instance :after ((instance xform)
                                         &key
                                           xform/translate
                                           xform/translate/inc
                                           xform/rotate
                                           xform/rotate/inc
                                           xform/scale
                                           xform/scale/inc)
  (when xform/translate
    (initialize-translation instance xform/translate))
  (when xform/translate/inc
    (initialize-translation/inc instance xform/translate/inc))
  (when xform/rotate
    (initialize-rotation instance xform/rotate))
  (when xform/rotate/inc
    (initialize-rotation/inc instance xform/rotate/inc))
  (when xform/scale
    (initialize-scaling instance xform/scale))
  (when xform/scale/inc
    (initialize-scaling/inc instance xform/scale/inc)))

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
  (set-uniforms (render/material entity) :model (xform/model entity)))

(defun translate-entity (entity vec &optional replace-p)
  (with-slots (%current) (xform/translation entity)
    (v3:+! %current (if replace-p v3:+zero+ %current) vec)))

(defun rotate-entity (entity vec &optional replace-p)
  (with-slots (%current) (xform/rotation entity)
    (q:rotate-euler! %current (if replace-p q:+id+ %current) vec)))

(defun scale-entity (entity vec &optional replace-p)
  (with-slots (%current) (xform/scaling entity)
    (v3:+! %current (if replace-p v3:+zero+ %current) vec)))
