(in-package #:pyx)

;;;; TODO: This is wrong. Do not use OBB's until this is investigated.

(defclass shape/obb (shape)
  ((%world-center :reader world-center
                  :initform (v3:zero))
   (%axes :reader axes
          :initform (m3:zero))
   (%half-widths :reader half-widths
                 :initform (v3:zero))))

(defun get-closest-point/obb-point (obb point)
  (with-slots (%entity %world-center %axes %half-widths) obb
    (let* ((d (v3:- point %world-center))
           (q (v3:copy %world-center)))
      (dotimes (i 3)
        (let* ((e (aref %half-widths i))
               (dist (a:clamp (v3:dot d (m3:get-column %axes i)) (- e) e)))
          (v3:+! q q (v3:scale (m3:get-column %axes i) dist))))
      q)))

(defun make-obb-obb-rotation (obb1 obb2)
  (let ((axes1 (axes obb1))
        (axes2 (axes obb2)))
    (m3:with-components ((a (m3:id))
                         (b (m3:id)))
      (psetf a00 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 0))
             a10 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 0))
             a20 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 0))
             a01 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 1))
             a11 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 1))
             a21 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 1))
             a02 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 2))
             a12 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 2))
             a22 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 2))
             b00 (+ (abs a00) 1e-7)
             b10 (+ (abs a10) 1e-7)
             b20 (+ (abs a20) 1e-7)
             b01 (+ (abs a01) 1e-7)
             b11 (+ (abs a11) 1e-7)
             b21 (+ (abs a21) 1e-7)
             b02 (+ (abs a02) 1e-7)
             b12 (+ (abs a12) 1e-7)
             b22 (+ (abs a22) 1e-7))
      (values a b))))

(defun make-obb-obb-translation (obb1 obb2)
  (let ((axes1 (axes obb1))
        (translation (v3:- (world-center obb2) (world-center obb1))))
    (v3:vec (v3:dot translation (m3:get-column axes1 0))
            (v3:dot translation (m3:get-column axes1 1))
            (v3:dot translation (m3:get-column axes1 2)))))

(defmethod update-shape ((shape shape/obb))
  (with-slots (%entity %center %world-center %axes %half-widths) shape
    (let* ((scale (current (xform/scaling %entity)))
           (min (transform-point %entity (v3:+ %center (v3:scale scale -2f0))))
           (max (transform-point %entity (v3:+ %center (v3:scale scale 2f0))))
           (axes (m4:rotation-to-mat3
                  (m4:normalize-rotation
                   (xform/model %entity))))
           (center (v3:lerp min max 0.5))
           (diagonal (v3:- max center))
           (half-widths (v3:vec (v3:dot diagonal (m3:get-column axes 0))
                                (v3:dot diagonal (m3:get-column axes 1))
                                (v3:dot diagonal (m3:get-column axes 2)))))
      (setf %world-center center
            %axes axes
            %half-widths half-widths))))
