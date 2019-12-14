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

(defun initialize-translation (entity &optional initial velocity)
  (with-slots (%current %previous %incremental) (xform/translation entity)
    (when initial
      (setf %current initial
            %previous (v3:copy %current)))
    (when velocity
      (setf %incremental velocity))))

(defun initialize-rotation (entity &optional initial velocity)
  (with-slots (%current %previous %incremental) (xform/rotation entity)
    (when initial
      (setf %current initial
            %previous (q:copy %current)))
    (when velocity
      (setf %incremental velocity))))

(defun initialize-scaling (entity &optional initial velocity)
  (with-slots (%current %previous %incremental) (xform/scaling entity)
    (when initial
      (setf %current (etypecase initial
                       (v3:vec initial)
                       (single-float (v3:vec initial initial initial)))
            %previous (v3:copy %current)))
    (when velocity
      (setf %incremental velocity))))

(defmethod shared-initialize :after ((instance xform) slot-names
                                     &key
                                       xform/translate
                                       xform/translate/velocity
                                       xform/rotate
                                       xform/rotate/velocity
                                       xform/scale
                                       xform/scale/velocity)
  (initialize-translation instance xform/translate xform/translate/velocity)
  (initialize-rotation instance xform/rotate xform/rotate/velocity)
  (initialize-scaling instance xform/scale xform/scale/velocity))

(defmethod reinitialize-instance :after ((instance xform)
                                         &key
                                           xform/translate
                                           xform/translate/velocity
                                           xform/rotate
                                           xform/rotate/velocity
                                           xform/scale
                                           xform/scale/velocity)
  (shared-initialize instance nil
                     :xform/translate xform/translate
                     :xform/translate/velocity xform/translate/velocity
                     :xform/rotate xform/rotate
                     :xform/rtoate/velocity xform/rotate/velocity
                     :xform/scale xform/scale
                     :xform/scale/velocity xform/scale/velocity))

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

(defun translate-entity (entity vec &optional replace-p)
  (with-slots (%current) (xform/translation entity)
    (v3:+! %current (if replace-p v3:+zero+ %current) vec)))

(defun rotate-entity (entity vec &optional replace-p)
  (with-slots (%current) (xform/rotation entity)
    (q:rotate-euler! %current (if replace-p q:+id+ %current) vec)))

(defun scale-entity (entity vec &optional replace-p)
  (with-slots (%current) (xform/scaling entity)
    (v3:+! %current (if replace-p v3:+zero+ %current) vec)))

;;; entity hooks

(define-hook :entity-update (entity xform)
  (transform-node entity))

(define-hook :entity-render (entity xform)
  (set-uniforms (render/current-material entity) :model xform/model))
