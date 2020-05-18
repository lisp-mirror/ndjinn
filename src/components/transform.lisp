(in-package #:net.mfiano.lisp.pyx)

(define-component transform ()
  ((%transform/translation :reader transform/translation
                           :initform (make-translate-state))
   (%transform/rotation :reader transform/rotation
                        :initform (make-rotate-state))
   (%transform/scale :reader transform/scale
                     :initform (make-scale-state))
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
  (initialize-translation (transform/translation instance)
                          transform/translate
                          transform/translate/velocity)
  (initialize-rotation (transform/rotation instance)
                       transform/rotate
                       transform/rotate/velocity)
  (initialize-scale (transform/scale instance)
                    transform/scale
                    transform/scale/velocity))

(defun transform-node (entity)
  (let ((delta (float =delta-time= 1f0)))
    (transform-node/vector (transform/scale entity) delta)
    (transform-node/quaternion (transform/rotation entity) delta)
    (transform-node/vector (transform/translation entity) delta)))

(defun resolve-local (entity factor)
  (let ((translation (transform/translation entity))
        (rotation (transform/rotation entity))
        (scale (transform/scale entity)))
    (symbol-macrolet ((local (transform/local entity)))
      (interpolate-vector scale factor)
      (interpolate-quaternion rotation factor)
      (interpolate-vector translation factor)
      (m4:copy! local (q:to-mat4 (interpolated rotation)))
      (m4:*! local local (m4:set-scale m4:+id+ (interpolated scale)))
      (m4:set-translation! local local (interpolated translation)))))

(defun resolve-model (entity alpha)
  (u:when-let ((parent (node/parent entity)))
    (resolve-local entity alpha)
    (m4:*! (transform/model entity)
           (transform/model parent)
           (transform/local entity))))

(defun resolve-normal-matrix (entity)
  (u:if-let ((camera (get-current-camera)))
    (m4:transpose (m4:invert (m4:* (camera/view camera)
                                   (transform/model entity))))
    m4:+id+))

(define-entity-hook :pre-render (entity transform)
  (set-uniforms entity :model transform/model))

(defun get-rotation (entity)
  (current (transform/rotation entity)))

(defun get-scale (entity)
  (current (transform/scale entity)))

(defun get-translation (entity)
  (current (transform/translation entity)))

(defun translate-entity (entity vec &key replace instant)
  (let ((state (transform/translation entity)))
    (symbol-macrolet ((current (current state)))
      (v3:+! current (if replace v3:+zero+ current) vec)
      (when instant
        (v3:copy! (previous state) current)))))

(defun translate-entity/velocity (entity axis rate)
  (let ((state (transform/translation entity)))
    (setf (incremental state) (math:make-velocity axis rate))))

(defun rotate-entity (entity quat &key replace instant)
  (let ((state (transform/rotation entity)))
    (symbol-macrolet ((current (current state)))
      (q:rotate! current (if replace q:+id+ current) quat)
      (when instant
        (q:copy! (previous state) current)))))

(defun rotate-entity/velocity (entity axis rate)
  (let ((state (transform/rotation entity)))
    (setf (incremental state) (math:make-velocity axis rate))))

(defun scale-entity (entity vec &key replace instant)
  (let ((state (transform/scale entity)))
    (symbol-macrolet ((current (current state)))
      (v3:+! current (if replace v3:+zero+ current) vec)
      (when instant
        (v3:copy! (previous state) current)))))

(defun scale-entity/velocity (entity axis rate)
  (let ((state (transform/scale entity)))
    (setf (incremental state) (math:make-velocity axis rate))))

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
