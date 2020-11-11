(in-package #:ndjinn)

(define-component animate ()
  ((%animate/sequence :reader animate/sequence
                      :initform (dll:make-list))))

;;; entity hooks

(define-entity-hook :update (entity animate)
  (process-animations entity))
