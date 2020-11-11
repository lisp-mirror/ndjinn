(in-package #:ndjinn)

(defstruct (collider-shape
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (type nil :type symbol)
  entity
  (center (v3:vec) :type v3:vec)
  (update-func (constantly nil) :type function))

(defun make-collider-shape (entity shape-spec)
  (destructuring-bind (type . args) (u:ensure-list shape-spec)
    (let ((constructor (ecase type
                         (sphere #'make-collider-shape/sphere)
                         (box #'make-collider-shape/box))))
      (apply constructor :type type :entity entity args))))

(defun update-collider-shape (collider)
  (let ((shape (collider/shape collider)))
    (funcall (collider-shape-update-func shape) shape)))
