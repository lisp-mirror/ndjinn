(in-package #:pyx)

(defgeneric handle-queued-event (purpose event-type data)
  (:method (purpose event-type data)
    (error "Unhandled queue event type ~s for queue purpose ~a."
           event-type purpose)))

(defmacro define-event-handler (purpose event-type &optional func)
  `(defmethod handle-queued-event ((purpose (eql ,purpose))
                                   (event-type (eql ,event-type))
                                   data)
     (funcall ,@(when func `(#',func)) data)))

(define-event-handler :entity-flow :prefab-create)

(define-event-handler :entity-flow :component-add)

(define-event-handler :entity-flow :component-add-hook)

(define-event-handler :entity-flow :component-modify)

(define-event-handler :entity-flow :component-remove)

(define-event-handler :entity-flow :entity-remove)

(define-event-handler :recompile :shader recompile-shaders)

(define-event-handler :recompile :material recompile-materials)
