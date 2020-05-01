(in-package #:pyx.component)

(pyx:define-component transform ()
  ((%transform/translation :reader transform/translation
                           :initform (pyx::make-translate-state))
   (%transform/rotation :reader transform/rotation
                        :initform (pyx::make-rotate-state))
   (%transform/scale :reader transform/scale
                     :initform (pyx::make-scale-state))
   (%transform/local :reader transform/local
                     :initform (m4:mat 1))
   (%transform/model :reader transform/model
                     :initform (m4:mat 1))
   (%transform/normal-matrix :reader transform/normal-matrix
                             :initform (m4:mat 1)))
  (:sorting :after node)
  (:static t))

(defmethod shared-initialize :after ((instance transform) slot-names
                                     &key
                                       transform/translate
                                       transform/translate/velocity
                                       transform/rotate
                                       transform/rotate/velocity
                                       transform/scale
                                       transform/scale/velocity)
  (pyx::initialize-translation (transform/translation instance)
                               transform/translate
                               transform/translate/velocity)
  (pyx::initialize-rotation (transform/rotation instance)
                            transform/rotate
                            transform/rotate/velocity)
  (pyx::initialize-scale (transform/scale instance)
                         transform/scale
                         transform/scale/velocity))

(defun transform-node (entity)
  (let ((delta pyx::=delta-time=))
    (pyx::transform-node/vector (transform/scale entity) delta)
    (pyx::transform-node/quaternion (transform/rotation entity) delta)
    (pyx::transform-node/vector (transform/translation entity) delta)))

(defun resolve-local (entity factor)
  (let ((translation (transform/translation entity))
        (rotation (transform/rotation entity))
        (scale (transform/scale entity)))
    (symbol-macrolet ((local (transform/local entity)))
      (pyx::interpolate-vector scale factor)
      (pyx::interpolate-quaternion rotation factor)
      (pyx::interpolate-vector translation factor)
      (m4:copy! local (q:to-mat4 (pyx::interpolated rotation)))
      (m4:*! local local (m4:set-scale m4:+id+ (pyx::interpolated scale)))
      (m4:set-translation! local local (pyx::interpolated translation)))))

(defun resolve-model (entity alpha)
  (a:when-let ((parent (node/parent entity)))
    (resolve-local entity alpha)
    (m4:*! (transform/model entity)
           (transform/model parent)
           (transform/local entity))))

(defun resolve-normal-matrix (entity)
  (a:if-let ((camera (get-current-camera)))
    (m4:transpose (m4:invert (m4:* (camera/view camera)
                                   (transform/model entity))))
    m4:+id+))

(pyx:define-entity-hook :pre-render (entity transform)
  (pyx:set-uniforms entity :model transform/model))

(defun get-rotation (entity)
  (pyx::current (transform/rotation entity)))

(defun get-scale (entity)
  (pyx::current (transform/scale entity)))

(defun get-translation (entity)
  (pyx::current (transform/translation entity)))

(defun translate-entity (entity vec &key replace instant)
  (let ((state (transform/translation entity)))
    (symbol-macrolet ((current (pyx::current state)))
      (v3:+! current (if replace v3:+zero+ current) vec)
      (when instant
        (v3:copy! (pyx::previous state) current)))))

(defun translate-entity/velocity (entity axis rate)
  (let ((state (transform/translation entity)))
    (setf (pyx::incremental state) (math:make-velocity axis rate))))

(defun rotate-entity (entity quat &key replace instant)
  (let ((state (transform/rotation entity)))
    (symbol-macrolet ((current (pyx::current state)))
      (q:rotate! current (if replace q:+id+ current) quat)
      (when instant
        (q:copy! (pyx::previous state) current)))))

(defun rotate-entity/velocity (entity axis rate)
  (let ((state (transform/rotation entity)))
    (setf (pyx::incremental state) (math:make-velocity axis rate))))

(defun scale-entity (entity vec &key replace instant)
  (let ((state (transform/scale entity)))
    (symbol-macrolet ((current (pyx::current state)))
      (v3:+! current (if replace v3:+zero+ current) vec)
      (when instant
        (v3:copy! (pyx::previous state) current)))))

(defun scale-entity/velocity (entity axis rate)
  (let ((state (transform/scale entity)))
    (setf (pyx::incremental state) (math:make-velocity axis rate))))

(defun transform-point (entity point &key (space :model))
  (let ((model (transform/model entity)))
    (v3:vec
     (ecase space
       (:model (m4:*v4 model (v4:vec point 1)))
       (:world (m4:*v4 (m4:invert model) (v4:vec point 1)))))))

(defun transform-vector (entity vector &key (space :model))
  (let ((model (transform/model entity)))
    (v3:vec
     (ecase space
       (:model (m4:*v4 model (v4:vec vector)))
       (:world (m4:*v4 (m4:invert model) (v4:vec vector)))))))

(defun transform-direction (entity direction &key (space :model))
  (let ((model (transform/model entity)))
    (m4:normalize-rotation! model model)
    (v3:vec
     (ecase space
       (:model (m4:*v4 model (v4:vec direction)))
       (:world (m4:*v4 (m4:invert model) (v4:vec direction)))))))