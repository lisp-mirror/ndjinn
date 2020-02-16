(in-package #:pyx.component)

(pyx:define-component animate ()
  ((%animate/states :reader animate/states
                    :initform (dll:make-dlist :test #'eq))
   (%animate/sequence :reader animate/sequence
                      :initarg :animate/sequence
                      :initform nil))
  (:sorting :before (mesh sprite) :after render))

;;; entity hooks

(pyx:define-entity-hook :attach (instance animate)
  (a:when-let ((sequence (u:href pyx::=animation-sequences= animate/sequence)))
    (funcall sequence instance)))

(pyx:define-entity-hook :pre-render (entity animate)
  (pyx::process-animation-states entity))
