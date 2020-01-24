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
                       (real (v3:vec initial)))
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

;;; user protocol

(defun translate-entity (entity vec &key replace-p instant-p)
  (with-slots (%previous %current) (xform/translation entity)
    (v3:+! %current (if replace-p v3:+zero+ %current) vec)
    (when instant-p
      (v3:copy! %previous %current))))

(defun translate-entity/velocity (entity axis rate)
  (with-slots (%incremental) (xform/translation entity)
    (setf %incremental (math:make-velocity axis rate))))

(defun rotate-entity (entity quat &key replace-p instant-p)
  (with-slots (%previous %current) (xform/rotation entity)
    (q:rotate! %current (if replace-p q:+id+ %current) quat)
    (when instant-p
      (q:copy! %previous %current))))

(defun rotate-entity/velocity (entity axis rate)
  (with-slots (%incremental) (xform/rotation entity)
    (setf %incremental (math:make-velocity axis rate))))

(defun scale-entity (entity vec &key replace-p instant-p)
  (with-slots (%previous %current) (xform/scaling entity)
    (v3:+! %current (if replace-p v3:+zero+ %current) vec)
    (when instant-p
      (v3:copy! %previous %current))))

(defun scale-entity/velocity (entity axis rate)
  (with-slots (%incremental) (xform/scaling entity)
    (setf %incremental (math:make-velocity axis rate))))

(defun transform-point (entity point &key (space :model))
  (v3:with-components ((v point))
    (~:.xyz
     (ecase space
       (:model (m4:*v4 (xform/model entity)
                       (v4:vec vx vy vz 1)))
       (:world (m4:*v4 (m4:invert (xform/model entity))
                       (v4:vec vx vy vz 1)))))))

(defun transform-vector (entity vector &key (space :model))
  (v3:with-components ((v vector))
    (let ((model (m4:copy (xform/model entity))))
      (m4:set-translation! model model v3:+zero+)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun transform-direction (entity direction &key (space :model))
  (v3:with-components ((v direction))
    (let ((model (m4:copy (xform/model entity))))
      (m4:set-translation! model model v3:+zero+)
      (m4:normalize-rotation! model model)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

;;; entity hooks

(define-hook :update (entity xform)
  (transform-node entity))

(define-hook :pre-render (entity xform)
  (set-uniforms entity :model xform/model))
