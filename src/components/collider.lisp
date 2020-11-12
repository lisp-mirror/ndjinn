(in-package #:ndjinn)

(define-component collider ()
  ((%collider/shape :accessor collider/shape
                    :initarg :collider/shape
                    :initform 'sphere)
   (%collider/owner :accessor collider/owner
                    :initarg :collider/owner
                    :initform nil)
   (%collider/layer :reader collider/layer
                    :initarg :collider/layer)
   (%collider/visualize :reader collider/visualize
                        :initarg :collider/visualize
                        :initform nil)
   (%collider/pickable :accessor collider/pickable
                       :initarg :collider/pickable
                       :initform t)
   (%collider/continuable :reader collider/continuable
                          :initarg :collider/continuable
                          :initform nil)
   (%collider/contact-count :accessor collider/contact-count
                            :initform 0)
   (%collider/hit-p :accessor collider/hit-p
                    :initform nil))
  (:order :after render))

(defun initialize-collider-visualization (collider)
  (when (or (has-component-p collider 'mesh)
            (has-component-p collider 'render))
    (error "Entity ~s has a collider to be visualized, but it must not have ~
              a mesh or render component attached." collider))
  (attach-component collider 'mesh
                    :mesh/asset "meshes/colliders.glb"
                    :mesh/name (format nil "~(~a~)"
                                       (collider-shape-type
                                        (collider/shape collider))))
  (attach-component collider 'render :render/materials '(collider)))

(defun %on-collision-enter (contact1 contact2)
  (incf (collider/contact-count contact1))
  (when (plusp (collider/contact-count contact1))
    (setf (collider/hit-p contact1) t))
  (when (plusp (collider/contact-count contact2))
    (setf (collider/hit-p contact2) t))
  (on-collision-enter (collider/layer contact1)
                      (collider/owner contact1)
                      (collider/layer contact2)
                      (collider/owner contact2)))

(defun %on-collision-continue (contact1 contact2)
  (on-collision-continue (collider/layer contact1)
                         (collider/owner contact1)
                         (collider/layer contact2)
                         (collider/owner contact2)))

(defun %on-collision-exit (contact1 contact2)
  (decf (collider/contact-count contact1))
  (when (zerop (collider/contact-count contact1))
    (setf (collider/hit-p contact1) nil))
  (when (zerop (collider/contact-count contact2))
    (setf (collider/hit-p contact2) nil))
  (on-collision-exit (collider/layer contact1)
                     (collider/owner contact1)
                     (collider/layer contact2)
                     (collider/owner contact2)))

;;; component protocol

(define-entity-hook :attach (entity collider)
  (setf (collider/owner entity) (or (collider/owner entity) entity)
        (collider/shape entity) (make-collider-shape entity
                                                     (collider/shape entity)))
  (when (collider/visualize entity)
    (initialize-collider-visualization entity))
  (register-collider entity (collider/layer entity)))

(define-entity-hook :detach (entity collider)
  (setf (collider/owner entity) nil)
  (deregister-collider entity (collider/layer entity)))

(define-entity-hook :physics-update (entity collider)
  (update-collider-shape entity))

(define-entity-hook :pre-render (entity collider)
  (when (collider/visualize entity)
    (set-uniforms entity :contact (collider/hit-p entity))))
