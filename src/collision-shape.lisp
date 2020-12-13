(in-package #:ndjinn)

(defstruct (collision-shape
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (type :sphere :type (member :sphere :box))
  (entity nil :type (or mixin null))
  (center (v3:vec) :type v3:vec)
  (update-func (constantly nil) :type function))

(defun make-collision-shape (entity shape-spec)
  (destructuring-bind (type . args) (u:ensure-list shape-spec)
    (let ((constructor (ecase type
                         (:sphere #'make-collision-shape/sphere)
                         (:box #'make-collision-shape/box))))
      (apply constructor :type type :entity entity args))))

(defun update-collision-shape (collider)
  (let ((shape (collider/shape collider)))
    (funcall (collision-shape-update-func shape) shape)))
