(in-package #:pyx)

(defclass entity (node)
  ((%game-state :reader game-state
                :initarg :game-state)
   (%transform :reader transform
               :initarg :transform)))

(defun make-entity (game-state &optional type)
  (let ((type (or type 'entity)))
    (make-instance type
                   :game-state game-state
                   :transform (make-instance 'transform))))

(defgeneric update-entity (entity)
  (:method ((entity entity)))
  (:method :around ((entity entity))
    (transform-node entity)
    (call-next-method)))

(defgeneric render-entity (entity)
  (:method ((entity entity)))
  (:method :around ((entity entity))
    (resolve-model entity)
    (call-next-method)
    ;; TODO: draw here
    ))

(defun update-entities (game-state)
  (map-nodes game-state #'update-entity))

(defun render-entities (game-state)
  (map-nodes game-state #'render-entity))
