(in-package #:ndjinn)

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
   (%transform/scaling-matrix :reader transform/scaling-matrix
                              :initform (m4:mat 1))
   (%transform/normal-matrix :reader transform/normal-matrix
                             :initform (m4:mat 1)))
  (:order :after (node id))
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
                    transform/scale/velocity)
  (resolve-model instance (get-alpha)))

(defun transform-node (entity delta)
  (declare (optimize speed))
  (transform-node/vector (transform/scale entity) delta)
  (transform-node/quaternion (transform/rotation entity) delta)
  (transform-node/vector (transform/translation entity) delta))

(defun resolve-local (entity factor)
  (declare (optimize speed))
  (let ((rotation (transform/rotation entity))
        (scale (transform/scale entity))
        (translation (transform/translation entity))
        (local (transform/local entity))
        (scaling-matrix (transform/scaling-matrix entity)))
    (interpolate-quaternion rotation factor)
    (interpolate-vector scale factor)
    (interpolate-vector translation factor)
    (q:to-mat4! local (transform-state-interpolated rotation))
    (m4:set-scale! scaling-matrix m4:+id+ (transform-state-interpolated scale))
    (m4:*! local local scaling-matrix)
    (m4:set-translation! local
                         local
                         (transform-state-interpolated translation))))

(defun resolve-model (entity alpha)
  (declare (optimize speed))
  (u:when-let ((parent (node/parent entity))
               (local (resolve-local entity alpha)))
    (m4:*! (transform/model entity) (transform/model parent) local)))

(defun resolve-normal-matrix (entity)
  (declare (optimize speed))
  (let ((result (transform/normal-matrix entity)))
    (u:when-let ((camera (get-current-camera)))
      (m4:set-translation! result (transform/model entity) v3:+zero+)
      (m4:*! result (camera/view camera) result)
      (m4:invert! result result)
      (m4:transpose! result result))
    (m4:rotation-to-mat3 result)))

(defun get-translation (entity &key (space :local))
  (declare (optimize speed))
  (m4:get-translation
   (ecase space
     (:local (transform/local entity))
     (:world (transform/model entity)))))

(defun get-rotation (entity &key (space :local))
  (declare (optimize speed))
  (q:from-mat4
   (ecase space
     (:local (transform/local entity))
     (:world (transform/model entity)))))

(defun get-scale (entity &key (space :local))
  (declare (optimize speed))
  (m4:get-scale
   (ecase space
     (:local (transform/local entity))
     (:world (transform/model entity)))))

(defun translate-entity (entity vec &key replace instant force)
  (declare (optimize speed))
  (let* ((state (transform/translation entity))
         (current (transform-state-current state)))
    (v3:+! current (if replace v3:+zero+ current) vec)
    (when instant
      (delay-work (transform)
        (v3:copy! (transform-state-previous state) current)))
    (when force
      (resolve-model entity (get-alpha)))))

(defun clamp-translation (entity min max &key instant)
  (declare (optimize speed))
  (let* ((state (transform/translation entity))
         (current (transform-state-current state)))
    (v3:max! current current min)
    (v3:min! current current max)
    (when instant
      (delay-work (transform)
        (v3:copy! (transform-state-previous state) current)))))

(defun translate-entity/velocity (entity axis rate)
  (declare (optimize speed))
  (let ((state (transform/translation entity)))
    (setf (transform-state-incremental state) (v3:make-velocity axis rate))))

(defun rotate-entity (entity quat &key replace instant force)
  (let* ((state (transform/rotation entity))
         (current (transform-state-current state)))
    (q:rotate! current (if replace q:+id+ current) quat)
    (when instant
      (delay-work (transform)
        (q:copy! (transform-state-previous state) current)))
    (when force
      (resolve-model entity (get-alpha)))))

(defun rotate-entity/velocity (entity axis rate)
  (declare (optimize speed))
  (let ((state (transform/rotation entity)))
    (setf (transform-state-incremental state) (v3:make-velocity axis rate))))

(defun scale-entity (entity vec &key replace instant force)
  (declare (optimize speed))
  (let* ((state (transform/scale entity))
         (current (transform-state-current state)))
    (v3:+! current (if replace v3:+zero+ current) vec)
    (when instant
      (delay-work (transform)
        (v3:copy! (transform-state-previous state) current)))
    (when force
      (resolve-model entity (get-alpha)))))

(defun scale-entity/velocity (entity axis rate)
  (declare (optimize speed))
  (let ((state (transform/scale entity)))
    (setf (transform-state-incremental state) (v3:make-velocity axis rate))))

(defun transform-point (entity point &key (space :local))
  (declare (optimize speed))
  (let ((model (transform/model entity)))
    (v3:vec
     (ecase space
       (:local (m4:*v4 model (v4:vec point 1)))
       (:world (m4:*v4 (m4:invert model) (v4:vec point 1)))))))

(defun transform-vector (entity vector &key (space :local))
  (let ((model (m4:copy (transform/model entity))))
    (m4:set-translation! model model v3:+zero+)
    (v3:vec
     (ecase space
       (:local (m4:*v4 model (v4:vec vector 1)))
       (:world (m4:*v4 (m4:invert model) (v4:vec vector 1)))))))

(defun transform-direction (entity direction &key (space :local))
  (let ((model (m4:copy (transform/model entity))))
    (m4:set-translation! model model v3:+zero+)
    (m4:normalize-rotation! model model)
    (v3:vec
     (ecase space
       (:local (m4:*v4 model (v4:vec direction 1)))
       (:world (m4:*v4 (m4:invert model) (v4:vec direction 1)))))))

;;; Entity hooks

(define-entity-hook :pre-render (entity transform)
  (set-uniforms entity :model (transform/model entity)))
