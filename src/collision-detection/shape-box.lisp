(in-package #:net.mfiano.lisp.pyx)

(defclass collider-shape/box (collider-shape)
  ((%world-center :reader world-center
                  :initform (v3:vec))
   (%axes :reader axes
          :initform (m3:mat))
   (%half-widths :reader half-widths
                 :initform (v3:vec))
   (%min-extent :reader min-extent
                :initarg :min-extent
                :initform (v3:vec -0.5))
   (%max-extent :reader max-extent
                :initarg :max-extent
                :initform (v3:vec 0.5))))

(defun get-closest-point/box-point (box point)
  (with-slots (%entity %world-center %axes %half-widths) box
    (let* ((d (v3:- point %world-center))
           (q (v3:copy %world-center)))
      (dotimes (i 3)
        (let* ((e (aref %half-widths i))
               (dist (u:clamp (v3:dot d (m3:get-column %axes i)) (- e) e)))
          (v3:+! q q (v3:scale (m3:get-column %axes i) dist))))
      q)))

(defun make-box-box-rotation (box1 box2)
  (let ((axes1 (axes box1))
        (axes2 (axes box2)))
    (m3:with-components ((a (m3:mat 1))
                         (b (m3:mat 1)))
      (psetf a00 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 0))
             a10 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 0))
             a20 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 0))
             a01 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 1))
             a11 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 1))
             a21 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 1))
             a02 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 2))
             a12 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 2))
             a22 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 2)))
      (setf b00 (+ (abs a00) 1e-7)
            b10 (+ (abs a10) 1e-7)
            b20 (+ (abs a20) 1e-7)
            b01 (+ (abs a01) 1e-7)
            b11 (+ (abs a11) 1e-7)
            b21 (+ (abs a21) 1e-7)
            b02 (+ (abs a02) 1e-7)
            b12 (+ (abs a12) 1e-7)
            b22 (+ (abs a22) 1e-7))
      (values a b))))

(defun make-box-box-translation (box1 box2)
  (let ((axes1 (axes box1))
        (translation (v3:- (world-center box2) (world-center box1))))
    (v3:vec (v3:dot translation (m3:get-column axes1 0))
            (v3:dot translation (m3:get-column axes1 1))
            (v3:dot translation (m3:get-column axes1 2)))))

(defmethod update-collider-shape ((shape collider-shape/box))
  (with-slots (%entity %center %world-center %axes %half-widths
               %min-extent %max-extent)
      shape
    (let* ((min (transform-point %entity (v3:+ %center %min-extent)))
           (max (transform-point %entity (v3:+ %center %max-extent)))
           (center (v3:lerp min max 0.5))
           (axes (m4:rotation-to-mat3
                  (m4:normalize-rotation
                   (transform/model %entity))))
           (diagonal (v3:- max center))
           (half-widths (v3:vec (v3:dot diagonal (m3:get-column axes 0))
                                (v3:dot diagonal (m3:get-column axes 1))
                                (v3:dot diagonal (m3:get-column axes 2)))))
      (setf %world-center center
            %axes axes
            %half-widths half-widths))))
