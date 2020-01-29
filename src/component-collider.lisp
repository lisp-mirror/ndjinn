(in-package #:%pyx.component.collider)

(ent:define-component collider ()
  ((%shape :accessor shape
           :initarg :collider/shape
           :initform 'sphere)
   (%layer :reader layer
           :initarg :collider/layer)
   (%visualize :reader visualize
               :initarg :collider/visualize
               :initform t)
   (%target :accessor target
            :initarg :collider/target
            :initform nil)
   (%contact-count :accessor contact-count
                   :initform 0)
   (%hit-p :accessor hit-p
           :initform nil))
  (:sorting :after render))

(defun initialize-collider-visualization (entity)
  (when (visualize entity)
    (when (or (ent:has-component-p entity 'c/mesh:mesh)
              (ent:has-component-p entity 'c/render:render))
      (error "Entity ~s has a collider to be visualized, but it must not have ~
              a mesh or render component attached." entity))
    (ent:attach-component entity 'c/mesh:mesh
                          :mesh/file "colliders.glb"
                          :mesh/name (format nil "~(~a~)" (shape entity)))
    (ent:attach-component entity 'c/render:render
                          :render/materials '(ext:collider))))

(defmethod cd:%on-collision-enter ((contact1 collider) (contact2 collider))
  (let ((targets (cd:callback-entities
                  (scene:collision-system (ctx:current-scene)))))
    (incf (contact-count contact1))
    (when (plusp (contact-count contact1))
      (setf (hit-p contact1) t))
    (when (plusp (contact-count contact2))
      (setf (hit-p contact2) t))
    (dolist (entity (cd:get-collision-targets targets contact1))
      (cd:on-collision-enter (target contact1) (layer contact2) entity))))

(defmethod cd:%on-collision-continue ((contact1 collider) (contact2 collider))
  (let ((targets (cd:callback-entities
                  (scene:collision-system (ctx:current-scene)))))
    (dolist (entity (cd:get-collision-targets targets contact1))
      (cd:on-collision-continue (target contact1) (layer contact2) entity))))

(defmethod cd:%on-collision-exit ((contact1 collider) (contact2 collider))
  (let ((targets (cd:callback-entities
                  (scene:collision-system (ctx:current-scene)))))
    (decf (contact-count contact1))
    (when (zerop (contact-count contact1))
      (setf (hit-p contact1) nil))
    (when (zerop (contact-count contact2))
      (setf (hit-p contact2) nil))
    (dolist (entity (cd:get-collision-targets targets contact1))
      (cd:on-collision-exit (target contact1) (layer contact2) entity))))

;;; component protocol

(ent:define-entity-hook :attach (entity collider)
  (initialize-collider-visualization entity)
  (setf shape (cd:make-shape entity shape))
  (cd:register-collider entity layer))

(ent:define-entity-hook :detach (entity collider)
  (cd:deregister-collider entity layer)
  (setf target nil))

(ent:define-entity-hook :physics-update (entity collider)
  (cd:update-shape shape))

(ent:define-entity-hook :pre-render (entity collider)
  (mat:set-uniforms (c/render:current-material entity) :contact hit-p))
