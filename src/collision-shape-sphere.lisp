(in-package #:ndjinn)

(defstruct (collision-shape/sphere
            (:include collision-shape)
            (:constructor %make-collision-shape/sphere)
            (:conc-name sphere-)
            (:predicate nil)
            (:copier nil))
  (radius 1f0 :type u:f32))

(defun make-collision-shape/sphere (&rest args)
  (let ((shape (apply #'%make-collision-shape/sphere args)))
    (v3:with-components ((s (get-scale (collision-shape-entity shape)
                                       :space :world)))
      (setf (sphere-radius shape) (max sx sy sz))
      shape)))
