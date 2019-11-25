(in-package #:pyx)

(define-component animate ()
  ((%animate/states :reader animate/states
                    :initform (doubly-linked-list:make-dlist :test #'eq))
   (%animate/sequence :reader animate/sequence
                      :initarg :animate/sequence
                      :initform nil))
  (:sorting :before (mesh sprite) :after render))

(defmethod shared-initialize :after ((instance animate) slot-names &key)
  (with-slots (%animate/sequence) instance
    (a:when-let ((sequence (meta :animation-sequences %animate/sequence)))
      (funcall sequence instance))))

(defmethod on-render progn ((entity animate))
  (process-animation-states entity))
