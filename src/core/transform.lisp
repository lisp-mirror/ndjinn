(in-package #:net.mfiano.lisp.pyx)

(defstruct (transform-state
            (:predicate nil)
            (:copier nil))
  (previous (v3:vec) :type (or v3:vec q:quat))
  (current (v3:vec) :type (or v3:vec q:quat))
  (incremental (v3:vec) :type v3:vec)
  (incremental-delta (v3:vec) :type (or v3:vec q:quat))
  (interpolated (v3:vec) :type (or v3:vec q:quat)))

(defun make-translate-state ()
  (make-transform-state :previous (v3:vec)
                        :current (v3:vec)
                        :incremental (v3:vec)
                        :incremental-delta (v3:vec)
                        :interpolated (v3:vec)))

(defun make-rotate-state ()
  (make-transform-state :previous (q:quat 1)
                        :current (q:quat 1)
                        :incremental (v3:vec)
                        :incremental-delta (q:quat 1)
                        :interpolated (q:quat 1)))

(defun make-scale-state ()
  (make-transform-state :previous (v3:vec 1)
                        :current (v3:vec 1)
                        :incremental (v3:vec)
                        :incremental-delta (v3:vec)
                        :interpolated (v3:vec)))

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

(u:fn-> transform-node/vector (transform-state single-float) (values))
(defun transform-node/vector (state delta)
  (declare (optimize speed))
  (v3:copy! (transform-state-previous state) (transform-state-current state))
  (v3:scale! (transform-state-incremental-delta state)
             (transform-state-incremental state)
             delta)
  (v3:+! (transform-state-current state)
         (transform-state-current state)
         (transform-state-incremental-delta state))
  (values))

(u:fn-> transform-node/quaternion (transform-state single-float) (values))
(defun transform-node/quaternion (state delta)
  (declare (optimize speed))
  (q:copy! (transform-state-previous state) (transform-state-current state))
  (q:from-velocity! (transform-state-incremental-delta state)
                    (transform-state-incremental state)
                    delta)
  (q:rotate! (transform-state-current state)
             (transform-state-current state)
             (transform-state-incremental-delta state))
  (values))
