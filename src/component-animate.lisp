(in-package #:pyx)

(define-component animate ()
  ((%animate/states :reader animate/states
                    :initform (doubly-linked-list:make-dlist :test #'eq))
   (%animate/sequence :reader animate/sequence
                      :initarg :animate/sequence
                      :initform nil))
  (:sorting :before (mesh sprite) :after render))

;;; entity hooks

(define-hook :attach (instance animate)
  (a:when-let ((sequence (meta :animation-sequences animate/sequence)))
    (funcall sequence instance)))

(define-hook :pre-render (entity animate)
  (process-animation-states entity))
