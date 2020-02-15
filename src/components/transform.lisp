(in-package #:%pyx.component.transform)

(ent:define-component transform ()
  ((%translation :reader translation
                 :initform (tfm:make-translate-state))
   (%rotation :reader rotation
              :initform (tfm:make-rotate-state))
   (%scale :reader scale
           :initform (tfm:make-scale-state))
   (%local :reader local
           :initform (m4:mat 1))
   (%model :reader model
           :initform (m4:mat 1))
   (%normal-matrix :reader normal-matrix
                   :initform (m4:mat 1)))
  (:sorting :after c/node:node)
  (:static t))

(defmethod shared-initialize :after ((instance transform) slot-names
                                     &key
                                       transform/translate
                                       transform/translate/velocity
                                       transform/rotate
                                       transform/rotate/velocity
                                       transform/scale
                                       transform/scale/velocity)
  (tfm:initialize-translation (translation instance)
                              transform/translate
                              transform/translate/velocity)
  (tfm:initialize-rotation (rotation instance)
                           transform/rotate
                           transform/rotate/velocity)
  (tfm:initialize-scale (scale instance)
                        transform/scale
                        transform/scale/velocity))

(defun transform-node (entity)
  (let ((delta cfg:=delta-time=)
        (frame-time (clock:get-frame-time)))
    (tfm:transform-node/vector (scale entity) delta frame-time)
    (tfm:transform-node/quaternion (rotation entity) delta frame-time)
    (tfm:transform-node/vector (translation entity) delta frame-time)))

(defun resolve-local (entity factor)
  (let ((translation (translation entity))
        (rotation (rotation entity))
        (scale (scale entity)))
    (symbol-macrolet ((local (local entity)))
      (tfm:interpolate-vector scale factor)
      (tfm:interpolate-quaternion rotation factor)
      (tfm:interpolate-vector translation factor)
      (m4:copy! local (q:to-mat4 (tfm:interpolated rotation)))
      (m4:*! local local (m4:set-scale m4:+id+ (tfm:interpolated scale)))
      (m4:set-translation! local local (tfm:interpolated translation)))))

(defun resolve-model (entity alpha)
  (a:when-let ((parent (c/node:parent entity)))
    (resolve-local entity alpha)
    (m4:*! (model entity)
           (model parent)
           (local entity))))

(defun resolve-normal-matrix (entity)
  (a:if-let ((camera (c/camera:get-current-camera)))
    (m4:transpose (m4:invert (m4:* (c/camera:view camera) (model entity))))
    m4:+id+))

(ent:define-entity-hook :pre-render (entity transform)
  (mat:set-uniforms (c/render:current-material entity)
                    :model model))

(defun get-rotation (entity)
  (tfm:current (rotation entity)))

(defun get-scale (entity)
  (tfm:current (scale entity)))

(defun get-translation (entity)
  (tfm:current (translation entity)))

(defun translate-entity (entity vec &key replace-p instant-p)
  (let ((state (translation entity)))
    (v3:+! (tfm:current state)
           (if replace-p v3:+zero+ (tfm:current state))
           vec)
    (when instant-p
      (v3:copy! (tfm:previous state)
                (tfm:current state)))))

(defun translate-entity/velocity (entity axis rate)
  (let ((state (translation entity)))
    (setf (tfm:incremental state) (math:make-velocity axis rate))))

(defun rotate-entity (entity quat &key replace-p instant-p)
  (let ((state (rotation entity)))
    (q:rotate! (tfm:current state)
               (if replace-p q:+id+ (tfm:current state))
               quat)
    (when instant-p
      (q:copy! (tfm:previous state)
               (tfm:current state)))))

(defun rotate-entity/velocity (entity axis rate)
  (let ((state (rotation entity)))
    (setf (tfm:incremental state) (math:make-velocity axis rate))))

(defun scale-entity (entity vec &key replace-p instant-p)
  (let ((state (scale entity)))
    (v3:+! (tfm:current state)
           (if replace-p v3:+zero+ (tfm:current state))
           vec)
    (when instant-p
      (v3:copy! (tfm:previous state)
                (tfm:current state)))))

(defun scale-entity/velocity (entity axis rate)
  (let ((state (scale entity)))
    (setf (tfm:incremental state) (math:make-velocity axis rate))))

(defun transform-point (entity point &key (space :model))
  (let ((model (model entity)))
    (v3:with-components ((v point))
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun transform-vector (entity vector &key (space :model))
  (v3:with-components ((v vector))
    (let ((model (m4:copy (model entity))))
      (m4:set-translation! model model v3:+zero+)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))

(defun transform-direction (entity direction &key (space :model))
  (v3:with-components ((v direction))
    (let ((model (m4:copy (model entity))))
      (m4:set-translation! model model v3:+zero+)
      (m4:normalize-rotation! model model)
      (~:.xyz
       (ecase space
         (:model (m4:*v4 model (v4:vec vx vy vz 1)))
         (:world (m4:*v4 (m4:invert model) (v4:vec vx vy vz 1))))))))
