(in-package #:%pyx.component.animate)

(ent:define-component animate ()
  ((%states :reader states
            :initform (dll:make-dlist :test #'eq))
   (%sequence :reader animation-sequence
              :initarg :animate/sequence
              :initform nil))
  (:sorting :before (c/mesh:mesh c/sprite:sprite) :after c/render:render))

;;; entity hooks

(ent:define-entity-hook :attach (instance animate)
  (a:when-let ((sequence (u:href meta:=animation-sequences=
                                 animation-sequence)))
    (funcall sequence instance)))

(ent:define-entity-hook :pre-render (entity animate)
  (anim:process-animation-states entity))
