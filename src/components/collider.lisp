(in-package #:net.mfiano.lisp.pyx)

(define-component collider ()
  ((%collider/shape :accessor collider/shape
                    :initarg :collider/shape
                    :initform 'sphere)
   (%collider/layer :reader collider/layer
                    :initarg :collider/layer)
   (%collider/visualize :reader collider/visualize
                        :initarg :collider/visualize
                        :initform t)
   (%collider/target :accessor collider/target
                     :initarg :collider/target
                     :initform nil)
   (%collider/contact-count :accessor collider/contact-count
                            :initform 0)
   (%collider/hit-p :accessor collider/hit-p
                    :initform nil))
  (:sorting :after render))

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
                                         (collider/shape entity))))
  (attach-component entity 'render :render/materials '(collider)))

(defmethod %on-collision-enter ((contact1 collider) (contact2 collider))
  (let ((targets (callback-entities
                  (collision-system (current-scene)))))
    (incf (collider/contact-count contact1))
    (when (plusp (collider/contact-count contact1))
      (setf (collider/hit-p contact1) t))
    (when (plusp (collider/contact-count contact2))
      (setf (collider/hit-p contact2) t))
    (dolist (entity (get-collision-targets targets contact1))
      (on-collision-enter (collider/target contact1)
                          (collider/layer contact2)
                          entity))))

(defmethod %on-collision-continue ((contact1 collider) (contact2 collider))
  (let ((targets (callback-entities
                  (collision-system (current-scene)))))
    (dolist (entity (get-collision-targets targets contact1))
      (on-collision-continue (collider/target contact1)
                             (collider/layer contact2)
                             entity))))

(defmethod %on-collision-exit ((contact1 collider) (contact2 collider))
  (let ((targets (callback-entities
                  (collision-system (current-scene)))))
    (decf (collider/contact-count contact1))
    (when (zerop (collider/contact-count contact1))
      (setf (collider/hit-p contact1) nil))
    (when (zerop (collider/contact-count contact2))
      (setf (collider/hit-p contact2) nil))
    (dolist (entity (get-collision-targets targets contact1))
      (on-collision-exit (collider/target contact1)
                              (collider/layer contact2)
                              entity))))

;;; component protocol

(define-entity-hook :attach (entity collider)
  (initialize-collider-visualization entity)
  (setf collider/shape (make-collider-shape entity collider/shape))
  (register-collider entity collider/layer))

(define-entity-hook :detach (entity collider)
  (deregister-collider entity collider/layer)
  (setf collider/target nil))

(define-entity-hook :physics-update (entity collider)
  (update-collider-shape collider/shape))

(define-entity-hook :pre-render (entity collider)
  (set-uniforms entity :contact collider/hit-p))
