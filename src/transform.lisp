(in-package #:pyx)

(defclass transform-state ()
  ((%current :accessor current
             :initarg :current)
   (%incremental :accessor incremental
                 :initarg :incremental)
   (%increment-delta :accessor incremental-delta
                     :initarg :incremental-delta)
   (%previous :accessor previous
              :initarg :previous)
   (%interpolated :accessor interpolated
                  :initarg :interpolated)))

(defclass transform-state-vector (transform-state) ()
  (:default-initargs :current (v3:zero)
                     :incremental (v3:zero)
                     :incremental-delta (v3:zero)
                     :previous (v3:zero)
                     :interpolated (v3:zero)))

(defclass transform-state-quaternion (transform-state) ()
  (:default-initargs :current (q:id)
                     :incremental (q:id)
                     :incremental-delta (q:id)
                     :previous (q:id)
                     :interpolated (q:id)))

(defun make-translation-state ()
  (make-instance 'transform-state-vector))

(defun make-rotation-state ()
  (make-instance 'transform-state-quaternion))

(defun make-scaling-state ()
  (make-instance 'transform-state-vector :current (v3:one)))

(defun interpolate-vector (state factor)
  (with-slots (%interpolated %current %previous) state
    (v3:lerp! %interpolated %previous %current factor)))

(defun interpolate-quaternion (state factor)
  (with-slots (%interpolated %current %previous) state
    (q:slerp! %interpolated %previous %current factor)))

(defclass transform ()
  ((%parent :accessor parent
            :initform nil)
   (%children :accessor children
              :initform nil)
   (%translation :reader translation
                 :initform (make-translation-state))
   (%rotation :reader rotation
              :initform (make-rotation-state))
   (%scaling :reader scaling
             :initform (make-scaling-state))
   (%local :reader local
           :initform (m4:id))
   (%model :reader model
           :initform (m4:id))))

(defun transform-node/vector (state delta)
  (with-slots (%previous %current %incremental-delta %incremental) state
    (v3:copy! %previous %current)
    (v3:scale! %incremental-delta %incremental delta)
    (v3:+! %current %current %incremental-delta)))

(defun transform-node/quaternion (state delta)
  (with-slots (%previous %current %incremental-delta %incremental) state
    (q:copy! %previous %current)
    (q:slerp! %incremental-delta q:+id+ %incremental delta)
    (q:rotate! %current %current %incremental-delta)))

(defun transform-node (game-state node)
  (let ((delta (clock-delta-time (clock game-state))))
    (transform-node/vector (scaling node) delta)
    (transform-node/quaternion (rotation node) delta)
    (transform-node/vector (translation node) delta)))

(defun resolve-local (node factor)
  (with-slots (%scaling %rotation %translation %local) node
    (interpolate-vector %scaling factor)
    (interpolate-quaternion %rotation factor)
    (interpolate-vector %translation factor)
    (m4:copy! %local (q:to-mat4 (interpolated %rotation)))
    (m4:*! %local %local (m4:set-scale m4:+id+ (interpolated %scaling)))
    (m4:set-translation! %local %local (interpolated %translation))))

(defun resolve-model (node factor)
  (a:when-let ((parent (parent node)))
    (resolve-local node factor)
    (m4:*! (model node) (model parent) (local node))))

(defun make-scene-tree (game-state)
  (setf (slot-value game-state '%scene-tree) (make-instance 'transform)))

(defun map-nodes (func parent)
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

(defun interpolate-transforms (game-state)
  (map-nodes
   (lambda (x) (resolve-model x (clock-interpolation-factor (clock game-state))))
   (scene-tree game-state)))
