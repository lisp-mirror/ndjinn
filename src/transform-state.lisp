(in-package #:ndjinn)

(defstruct (transform-state
            (:predicate nil)
            (:copier nil))
  (previous (v3:vec) :type (or v3:vec q:quat))
  (current (v3:vec) :type (or v3:vec q:quat))
  (incremental (v3:vec) :type v3:vec)
  (incremental-delta (v3:vec) :type (or v3:vec q:quat))
  (interpolated (v3:vec) :type (or v3:vec q:quat)))

(defun make-translate-state ()
  (make-transform-state))

(defun make-rotate-state ()
  (make-transform-state :previous (q:quat 1)
                        :current (q:quat 1)
                        :incremental-delta (q:quat 1)
                        :interpolated (q:quat 1)))

(defun make-scale-state ()
  (make-transform-state :previous (v3:vec 1)
                        :current (v3:vec 1)))

(defun initialize-translation (state &optional initial velocity)
  (when initial
    (setf (transform-state-current state) initial
          (transform-state-previous state) (v3:copy initial)))
  (when velocity
    (setf (transform-state-incremental state) velocity)))

(defun initialize-rotation (state &optional initial velocity)
  (when initial
    (setf (transform-state-current state) initial
          (transform-state-previous state) (q:copy initial)))
  (when velocity
    (setf (transform-state-incremental state) velocity)))

(defun initialize-scale (state &optional initial velocity)
  (when initial
    (let ((initial (etypecase initial
                     (v3:vec initial)
                     (real (v3:vec initial)))))
      (setf (transform-state-current state) initial
            (transform-state-previous state) (v3:copy initial))))
  (when velocity
    (setf (transform-state-incremental state) velocity)))

(u:fn-> interpolate-vector (transform-state single-float) v3:vec)
(u:defun-inline interpolate-vector (state factor)
  (declare (optimize speed))
  (v3:lerp! (transform-state-interpolated state)
            (transform-state-previous state)
            (transform-state-current state)
            factor))

(u:fn-> interpolate-quaternion (transform-state single-float) q:quat)
(u:defun-inline interpolate-quaternion (state factor)
  (declare (optimize speed))
  (q:slerp! (transform-state-interpolated state)
            (transform-state-previous state)
            (transform-state-current state)
            factor))

(u:fn-> transform-node/vector (transform-state single-float) null)
(defun transform-node/vector (state delta)
  (declare (optimize speed))
  (let ((current (transform-state-current state))
        (incremental-delta (transform-state-incremental-delta state)))
    (v3:copy! (transform-state-previous state) current)
    (v3:scale! incremental-delta (transform-state-incremental state) delta)
    (v3:+! current current incremental-delta)
    nil))

(u:fn-> transform-node/quaternion (transform-state single-float) null)
(defun transform-node/quaternion (state delta)
  (declare (optimize speed))
  (let ((current (transform-state-current state))
        (incremental-delta (transform-state-incremental-delta state)))
    (q:copy! (transform-state-previous state) current)
    (q:from-velocity! incremental-delta
                      (transform-state-incremental state)
                      delta)
    (q:rotate! current current incremental-delta)
    nil))
