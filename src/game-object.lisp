(in-package #:pyx)

(defclass game-object (node)
  ((%game-state :reader game-state
                :initarg :game-state)
   (%transform :reader transform
               :initarg :transform)))

(defun make-game-object (game-state &optional type)
  (let ((type (or type 'game-object)))
    (make-instance type
                   :game-state game-state
                   :transform (make-instance 'transform))))

(defgeneric update-game-object (game-object)
  (:method ((game-object game-object)))
  (:method :around ((game-object game-object))
    (transform-node game-object)
    (call-next-method)))

(defgeneric render-game-object (game-object)
  (:method ((game-object game-object)))
  (:method :around ((game-object game-object))
    (resolve-model game-object)
    (call-next-method)
    ;; TODO: draw here
    ))

(defun update-game-objects (game-state)
  (map-nodes game-state #'update-game-object))

(defun render-game-objects (game-state)
  (map-nodes game-state #'render-game-object))
