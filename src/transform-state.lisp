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

(defun make-translate-state ()
  (make-instance 'transform-state
                 :current (v3:vec)
                 :frame (v3:vec)
                 :incremental (v3:vec)
                 :incremental-delta (v3:vec)
                 :interpolated (v3:vec)
                 :previous (v3:vec)))

(defun make-rotate-state ()
  (make-instance 'transform-state
                 :current (q:quat 1)
                 :frame (q:quat 1)
                 :incremental (v3:vec)
                 :incremental-delta (q:quat 1)
                 :interpolated (q:quat 1)
                 :previous (q:quat 1)))

(defun make-scale-state ()
  (make-instance 'transform-state
                 :current (v3:vec 1)
                 :frame (v3:vec)
                 :incremental (v3:vec)
                 :incremental-delta (v3:vec)
                 :interpolated (v3:vec)
                 :previous (v3:vec 1)))

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
    (math:velocity->rotation! %incremental-delta %incremental delta)
    (q:rotate! %current %current %incremental-delta)
    (q:id! %frame)))
