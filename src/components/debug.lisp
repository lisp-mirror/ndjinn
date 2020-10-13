(in-package #:net.mfiano.lisp.pyx)

(define-component debug ()
  ((%debug/enabled :reader debug/enabled
                   :initform t)))

(define-entity-hook :update (entity debug)
  (when (debug/enabled entity)
    (cond
      ((on-button-enter :key :escape)
       (stop-engine))
      ((on-button-enter :mouse :left)
       (pick-entity)))))
