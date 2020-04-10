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
  (let ((delta pyx::=delta-time=)
        (frame-time (pyx:get-frame-time)))
    (pyx::transform-node/vector (transform/scale entity)
                                delta
                                frame-time)
    (pyx::transform-node/quaternion (transform/rotation entity)
                                    delta
                                    frame-time)
    (pyx::transform-node/vector (transform/translation entity)
                                delta
                                frame-time)))

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

(defun translate-entity (entity vec &key replace-p instant-p)
  (let ((state (transform/translation entity)))
    (v3:+! (pyx::current state)
           (if replace-p v3:+zero+ (pyx::current state))
           vec)
    (when instant-p
      (v3:copy! (pyx::previous state)
                (pyx::current state)))))

(defun translate-entity/velocity (entity axis rate)
  (let ((state (transform/translation entity)))
    (setf (pyx::incremental state) (math:make-velocity axis rate))))

(defun rotate-entity (entity quat &key replace-p instant-p)
  (let ((state (transform/rotation entity)))
    (q:rotate! (pyx::current state)
               (if replace-p q:+id+ (pyx::current state))
               quat)
    (when instant-p
      (q:copy! (pyx::previous state)
               (pyx::current state)))))

(defun rotate-entity/velocity (entity axis rate)
  (let ((state (transform/rotation entity)))
    (setf (pyx::incremental state) (math:make-velocity axis rate))))

(defun scale-entity (entity vec &key replace-p instant-p)
  (let ((state (transform/scale entity)))
    (v3:+! (pyx::current state)
           (if replace-p v3:+zero+ (pyx::current state))
           vec)
    (when instant-p
      (v3:copy! (pyx::previous state)
                (pyx::current state)))))

(defun scale-entity/velocity (entity axis rate)
  (let ((state (transform/scale entity)))
    (setf (pyx::incremental state) (math:make-velocity axis rate))))

(defun transform-point (entity point &key (space :model))
  (let ((model (transform/model entity)))
    (v3:with-components ((v point))
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun transform-vector (entity vector &key (space :model))
  (v3:with-components ((v vector))
    (let ((model (m4:copy (transform/model entity))))
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 0)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 0))))))))

(defun transform-direction (entity direction &key (space :model))
  (v3:with-components ((v direction))
    (let ((model (m4:copy (transform/model entity))))
      (m4:normalize-rotation! model model)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 0)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 0))))))))
