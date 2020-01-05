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
   (%collider/referent :reader collider/referent
                       :initarg :collider/referent
                       :initform nil)
   (%collider/contact-count :accessor collider/contact-count
                            :initform 0))
  (:sorting :after render))

(defgeneric collide-p (collider1 collider2)
  (:method ((collider1 collider) (collider2 collider))))

(defgeneric on-collision-enter (collider1 collider2)
  (:method ((collider1 collider) (collider2 collider))
    (on-collision-enter (collider/referent collider1) collider2))
  (:method :before ((collider1 collider) (collider2 collider))
    (when (eq collider1 (collider/referent collider1))
      (error "Collider referent cannot be the same collider."))))

(defgeneric on-collision-continue (collider1 collider2)
  (:method ((collider1 collider) (collider2 collider))
    (on-collision-continue (collider/referent collider1) collider2))
  (:method :before ((collider1 collider) (collider2 collider))
    (when (eq collider1 (collider/referent collider1))
      (error "Collider referent cannot be the same collider."))))

(defgeneric on-collision-exit (collider1 collider2)
  (:method ((collider1 collider) (collider2 collider))
    (on-collision-exit (collider/referent collider1) collider2))
  (:method :before ((collider1 collider) (collider2 collider))
    (when (eq collider1 (collider/referent collider1))
      (error "Collider referent cannot be the same collider."))))
