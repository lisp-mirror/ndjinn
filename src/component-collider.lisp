(in-package #:pyx)

(define-component collider ()
  ((%collider/label :reader collider/label
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
                            :initform 0))
  (:sorting :after render))

(defgeneric collide-p (collider1 collider2)
  (:method (collider1 collider2)))

;;; component protocol

(define-hook :entity-delete (entity collider)
  (setf (collider/referent entity) nil))

;;; user protocol

(defgeneric on-collision-enter (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) contact2)
    (incf (collider/contact-count contact1))
    (a:when-let ((referent (collider/referent contact1)))
      (when (eq contact1 referent)
        (error "Collider referent cannot be the same collider."))
      (on-collision-enter (collider/referent contact1) contact2))))

(defgeneric on-collision-continue (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) contact2)
    (a:when-let ((referent (collider/referent contact1)))
      (when (eq contact1 referent)
        (error "Collider referent cannot be the same collider."))
      (on-collision-continue (collider/referent contact1) contact2))))

(defgeneric on-collision-exit (contact1 contact2)
  (:method (contact1 contact2))
  (:method ((contact1 collider) contact2)
    (decf (collider/contact-count contact1))
    (a:when-let ((referent (collider/referent contact1)))
      (when (eq contact1 referent)
        (error "Collider referent cannot be the same collider."))
      (on-collision-exit (collider/referent contact1) contact2))))
