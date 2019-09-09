(in-package #:pyx)

(defclass transform-state ()
  ((%current :accessor current
             :initarg :current)
   (%frame :accessor frame
           :initarg :frame)
   (%incremental :accessor incremental
                 :initarg :incremental)
   (%incremental-delta :accessor incremental-delta
                       :initarg :incremental-delta)
   (%interpolated :accessor interpolated
                  :initarg :interpolated)
   (%previous :accessor previous
              :initarg :previous)))

(defclass transform-state-vector (transform-state) ()
  (:default-initargs :current (v3:zero)
                     :frame (v3:zero)
                     :incremental (v3:zero)
                     :incremental-delta (v3:zero)
                     :interpolated (v3:zero)
                     :previous (v3:zero)))

(defclass transform-state-quaternion (transform-state) ()
  (:default-initargs :current (q:id)
                     :frame (q:id)
                     :incremental (q:id)
                     :incremental-delta (q:id)
                     :interpolated (q:id)
                     :previous (q:id)))

(defclass transform ()
  ((%translation :reader translation
                 :initform (make-translation-state))
   (%rotation :reader rotation
              :initform (make-rotation-state))
   (%scaling :reader scaling
             :initform (make-scaling-state))
   (%local :reader local
           :initform (m4:id))
   (%model :reader model
           :initform (m4:id))))

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

(defun transform-node/vector (state delta frame-time)
  (with-slots (%frame %previous %current %incremental-delta %incremental) state
    (v3:copy! %previous %current)
    (v3:scale! %frame %frame frame-time)
    (v3:+! %current %current %frame)
    (v3:scale! %incremental-delta %incremental delta)
    (v3:+! %current %current %incremental-delta)
    (v3:zero! %frame)))

(defun transform-node/quaternion (state delta frame-time)
  (with-slots (%frame %previous %current %incremental-delta %incremental) state
    (q:copy! %previous %current)
    (q:slerp! %frame q:+id+ %frame frame-time)
    (q:rotate! %current %current %frame)
    (q:slerp! %incremental-delta q:+id+ %incremental delta)
    (q:rotate! %current %current %incremental-delta)
    (q:id! %frame)))

(defun transform-node (node)
  (with-slots (%scaling %rotation %translation) (transform node)
    (let* ((clock (clock (game-state node)))
           (delta (clock-delta-time clock))
           (frame-time (clock-frame-time clock)))
      (transform-node/vector %scaling delta frame-time)
      (transform-node/quaternion %rotation delta frame-time)
      (transform-node/vector %translation delta frame-time))))

(defun resolve-local (node factor)
  (with-slots (%scaling %rotation %translation %local) (transform node)
    (interpolate-vector %scaling factor)
    (interpolate-quaternion %rotation factor)
    (interpolate-vector %translation factor)
    (m4:copy! %local (q:to-mat4 (interpolated %rotation)))
    (m4:*! %local %local (m4:set-scale m4:+id+ (interpolated %scaling)))
    (m4:set-translation! %local %local (interpolated %translation))))

(defun resolve-model (node)
  (with-slots (%game-state %parent %transform) node
    (with-slots (%model %local) %transform
      (when %parent
        (resolve-local node (clock-interpolation-factor (clock %game-state)))
        (m4:*! %model (model (transform %parent)) %local)))))
