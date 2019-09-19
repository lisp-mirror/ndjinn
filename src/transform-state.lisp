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
                 :current (v3:zero)
                 :frame (v3:zero)
                 :incremental (v3:zero)
                 :incremental-delta (v3:zero)
                 :interpolated (v3:zero)
                 :previous (v3:zero)))

(defun make-rotate-state ()
  (make-instance 'transform-state
                 :current (q:id)
                 :frame (q:id)
                 :incremental (q:id)
                 :incremental-delta (q:id)
                 :interpolated (q:id)
                 :previous (q:id)))

(defun make-scale-state ()
  (make-instance 'transform-state
                 :current (v3:one)
                 :frame (v3:zero)
                 :incremental (v3:zero)
                 :incremental-delta (v3:zero)
                 :interpolated (v3:zero)
                 :previous (v3:one)))

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
    ;; TODO: fix with slerp
    (q:normalize! %frame (q:scale! %frame %frame frame-time))
    (q:rotate! %current %current %frame)
    ;; TODO: fix with slerp
    (q:normalize! %incremental-delta
                  (q:scale! %incremental-delta %incremental delta))
    (q:rotate! %current %current %incremental-delta)
    (q:id! %frame)))
