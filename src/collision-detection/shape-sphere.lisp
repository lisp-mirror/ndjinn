(in-package #:net.mfiano.lisp.pyx)

(defclass collider-shape/sphere (collider-shape)
  ((%radius :reader radius
            :initarg :radius
            :initform 1)))

(defmethod initialize-instance :after ((instance collider-shape/sphere) &key)
  (with-slots (%entity %radius) instance
    (v3:with-components ((s (get-scale %entity :space :world)))
      (setf %radius (max sx sy sz)))))
