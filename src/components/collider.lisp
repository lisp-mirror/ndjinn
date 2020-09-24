(in-package #:net.mfiano.lisp.pyx)

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
   (%collider/contact-count :accessor collider/contact-count
                            :initform 0)
   (%collider/hit-p :accessor collider/hit-p
                    :initform nil))
  (:type-order :after render))

(define-material collider ()
  (:shader shader:collider
   :uniforms (:hit-color (v4:vec 0 1 0 1)
              :miss-color (v4:vec 1 0 0 1))
   :features (:enable (:line-smooth)
              :polygon-mode :line
              :line-width 1.0)))

(defun initialize-collider-visualization (entity)
  (when (collider/visualize entity)
    (when (or (has-component-p entity 'mesh)
              (has-component-p entity 'render))
      (error "Entity ~s has a collider to be visualized, but it must not have ~
              a mesh or render component attached." entity))
    (attach-component entity 'mesh
                      :mesh/asset "meshes/colliders.glb"
                      :mesh/name (format nil "~(~a~)"
                                         (shape-type (collider/shape entity))))
    (attach-component entity 'render :render/materials '(collider))))

(defmethod %on-collision-enter ((contact1 collider) (contact2 collider))
  (incf (collider/contact-count contact1))
  (when (plusp (collider/contact-count contact1))
    (setf (collider/hit-p contact1) t))
  (when (plusp (collider/contact-count contact2))
    (setf (collider/hit-p contact2) t))
  (on-collision-enter (collider/layer contact1)
                      (collider/owner contact1)
                      (collider/layer contact2)
                      (collider/owner contact2)))

(defmethod %on-collision-continue ((contact1 collider) (contact2 collider))
  (on-collision-continue (collider/layer contact1)
                         (collider/owner contact1)
                         (collider/layer contact2)
                         (collider/owner contact2)))

(defmethod %on-collision-exit ((contact1 collider) (contact2 collider))
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
  (setf collider/owner (or collider/owner entity)
        collider/shape (make-collider-shape entity collider/shape))
  (initialize-collider-visualization entity)
  (register-collider entity collider/layer))

(define-entity-hook :detach (entity collider)
  (setf collider/owner nil)
  (deregister-collider entity collider/layer))

(define-entity-hook :physics-update (entity collider)
  (update-collider-shape collider/shape))

(define-entity-hook :pre-render (entity collider)
  (when collider/visualize
    (set-uniforms entity :contact collider/hit-p)))
