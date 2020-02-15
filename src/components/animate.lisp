(in-package #:%pyx.component)

(ent:define-component animate ()
  ((%animate/states :reader animate/states
                    :initform (dll:make-dlist :test #'eq))
   (%animate/sequence :reader animate/sequence
                      :initarg :animate/sequence
                      :initform nil))
  (:sorting :before (mesh sprite) :after render))

;;; entity hooks

(ent:define-entity-hook :attach (instance animate)
  (a:when-let ((sequence (u:href meta:=animation-sequences=
                                 animate/sequence)))
    (funcall sequence instance)))

(ent:define-entity-hook :pre-render (entity animate)
  (anim:process-animation-states entity))
