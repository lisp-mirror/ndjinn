(in-package #:pyx)

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

(defun initialize-collider-visualization (entity)
  (when (collider/visualize entity)
    (when (or (has-component-p entity 'mesh)
              (has-component-p entity 'render))
      (error "Entity ~s has a collider to be visualized, but it must not have ~
              a mesh or render component attached." entity))
    (attach-component entity 'mesh
                      :mesh/file "colliders.glb"
                      :mesh/name (format nil "~(~a~)" (collider/shape entity)))
    (attach-component entity 'render
                      :render/materials '(collider))))

(defgeneric %on-collision-enter (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) (contact2 collider))
    (incf (collider/contact-count contact1))
    (when (plusp (collider/contact-count contact1))
      (setf (collider/hit-p contact1) t))
    (when (plusp (collider/contact-count contact2))
      (setf (collider/hit-p contact2) t))
    (dolist (entity (get-collision-targets contact1))
      (on-collision-enter (collider/target contact1)
                          (collider/layer contact2)
                          entity))))

(defgeneric %on-collision-continue (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) (contact2 collider))
    (dolist (entity (get-collision-targets contact1))
      (on-collision-continue (collider/target contact1)
                             (collider/layer contact2)
                             entity))))

(defgeneric %on-collision-exit (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) (contact2 collider))
    (decf (collider/contact-count contact1))
    (when (zerop (collider/contact-count contact1))
      (setf (collider/hit-p contact1) nil))
    (when (zerop (collider/contact-count contact2))
      (setf (collider/hit-p contact2) nil))
    (dolist (entity (get-collision-targets contact1))
      (on-collision-exit (collider/target contact1)
                         (collider/layer contact2)
                         entity))))

(defgeneric on-collision-enter (target layer entity)
  (:method (target layer entity)))

(defgeneric on-collision-continue (target layer entity)
  (:method (target layer entity)))

(defgeneric on-collision-exit (target layer entity)
  (:method (target layer entity)))

(defgeneric on-collision-picked (target layer entity)
  (:method (target layer entity)))

(defmacro define-collision-hook (name (target &optional layer) &body body)
  (a:with-gensyms (target-symbol)
    (let ((hook-types '(:enter :continue :exit :picked)))
      `(progn
         ,@(unless (find name hook-types)
             `((error "Hook type must be one of: ~{~s~^, ~}" ',hook-types)))
         ,@(unless (symbolp target)
             `((error "Target of a collision hook must be a symbol: ~s."
                      ',target)))
         ,@(unless (symbolp layer)
             `((error "Layer of a collision hook must be a symbol: ~s."
                      ',layer)))
         (defmethod ,(a:format-symbol :pyx "ON-COLLISION-~a" name)
             ((,target-symbol (eql ',target)) (layer (eql ',layer)) ,target)
           ,@body)))))

;;; component protocol

(define-hook :attach (entity collider)
  (initialize-collider-visualization entity)
  (setf collider/shape (make-shape entity collider/shape))
  (register-collider entity))

(define-hook :detach (entity collider)
  (deregister-collider entity)
  (setf collider/target nil))

(define-hook :update (entity collider)
  (update-shape collider/shape))

(define-hook :pre-render (entity collider)
  (set-uniforms entity :contact collider/hit-p))
