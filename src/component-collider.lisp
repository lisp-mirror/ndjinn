(in-package #:pyx)

(define-component collider ()
  ((%collider/shape :accessor collider/shape
                    :initarg :collider/shape
                    :initform 'sphere)
   (%collider/label :reader collider/label
                    :initarg :collider/label)
   (%collider/center :reader collider/center
                     :initarg :collider/center
                     :initform (v3:zero))
   (%collider/visualize :reader collider/visualize
                        :initarg :collider/visualize
                        :initform nil)
   (%collider/referent :accessor collider/referent
                       :initarg :collider/referent
                       :initform nil)
   (%collider/contact-count :accessor collider/contact-count
                            :initform 0)
   (%collider/hit-p :accessor collider/hit-p
                    :initform nil))
  (:sorting :after render))

(ff:define-filtered-function collide-p (collider1 collider2)
  (:method (collider1 collider2)
    (declare (ignore collider1 collider2)))
  (:method ((collider1 collider) (collider2 collider))
    (error "There is no test function defined for collider shapes ~s and ~s."
           (class-name (class-of (collider/shape collider1)))
           (class-name (class-of (collider/shape collider2)))))
  (:filters (:shape (list (when (has-component-p collider1 'collider)
                            #'collider/shape)
                          (when (has-component-p collider2 'collider)
                            #'collider/shape)))))

;;; component protocol

(define-hook :attach (entity collider)
  (let ((shape (a:ensure-list collider/shape)))
    (destructuring-bind (shape . args) shape
      (unless (subtypep shape 'collider-shape)
        (error "Collider shape ~s is not defined." shape))
      (setf collider/shape (apply #'make-instance shape :collider entity args)))
    (register-collider entity)
    (when collider/visualize
      (attach-component entity 'render :render/materials '(collider)))))

(define-hook :detach (entity collider)
  (deregister-collider entity)
  (setf (collider/referent entity) nil))

(define-hook :pre-render (entity collider)
  (set-uniforms entity :contact collider/hit-p))

;;; user protocol

(defgeneric on-collision-enter (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) (contact2 collider))
    (incf (collider/contact-count contact1))
    (when (collider/visualize contact1)
      (setf (collider/hit-p contact1) t))
    (when (collider/visualize contact2)
      (setf (collider/hit-p contact2) t))
    (a:when-let ((referent (collider/referent contact1)))
      (when (eq contact1 referent)
        (error "Collider referent cannot be the same collider."))
      (on-collision-enter (collider/referent contact1) contact2))))

(defgeneric on-collision-continue (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) (contact2 collider))
    (a:when-let ((referent (collider/referent contact1)))
      (when (eq contact1 referent)
        (error "Collider referent cannot be the same collider."))
      (on-collision-continue (collider/referent contact1) contact2))))

(defgeneric on-collision-exit (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) (contact2 collider))
    (decf (collider/contact-count contact1))
    (when (collider/visualize contact1)
      (setf (collider/hit-p contact1) nil))
    (when (collider/visualize contact2)
      (setf (collider/hit-p contact2) nil))
    (a:when-let ((referent (collider/referent contact1)))
      (when (eq contact1 referent)
        (error "Collider referent cannot be the same collider."))
      (on-collision-exit (collider/referent contact1) contact2))))
