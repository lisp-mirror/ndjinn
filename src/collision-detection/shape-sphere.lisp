(in-package #:ndjinn)

(defstruct (collider-shape/sphere
            (:conc-name sphere-)
            (:constructor %make-collider-shape/sphere)
            (:include collider-shape)
            (:predicate nil)
            (:copier nil))
  (radius 1f0 :type single-float))

(defun make-collider-shape/sphere (&rest args)
  (let ((shape (apply #'%make-collider-shape/sphere args)))
    (v3:with-components ((s (get-scale (collider-shape-entity shape)
                                       :space :world)))
      (setf (sphere-radius shape) (max sx sy sz))
      shape)))
