(in-package #:net.mfiano.lisp.pyx)

(defclass mixin-class (standard-class)
  ((%classes :reader classes
             :initarg :classes
             :initform nil)))

(defclass mixin () ())

(defmethod c2mop:validate-superclass ((class mixin-class)
                                      (superclass standard-class))
  t)

(u:define-printer (mixin-class stream)
  (format stream "~{~a~^, ~}"
          (mapcar #'class-name
                  (cdr (c2mop:class-direct-superclasses mixin-class)))))

(defun get-mixin-class-names (mixin)
  (mapcar #'class-name
          (cdr (c2mop:class-direct-superclasses
                (class-of mixin)))))

(defun find-mixin-class (class)
  (etypecase class
    (symbol (find-class class))
    (class class)))

(defun find-mixin-class-names (class)
  (etypecase class
    (symbol class)
    (class (class-name class))))

(defun make-mixin-class-list (classes)
  (mapcar
   #'find-mixin-class
   (compute-component-type-order (mapcar #'find-mixin-class-names classes))))

(defun make-mixin-class (classes)
  (let* ((superclasses (cons (find-class 'mixin) classes))
         (class (make-instance 'mixin-class
                               :classes classes
                               :direct-superclasses superclasses)))
    (setf (u:href =meta/entity-mixins= (mapcar #'class-name classes))
          class)
    class))

(defun ensure-mixin-class (classes)
  (if (cdr classes)
      (or (u:href =meta/entity-mixins= (mapcar #'class-name classes))
          (make-mixin-class classes))
      (car classes)))

(defun add-mixin-class (mixin class &rest args)
  (let ((class (ensure-mixin-class
                (make-mixin-class-list
                 (typecase mixin
                   (mixin (list* class (classes (class-of mixin))))
                   (t (list class (class-of mixin))))))))
    (apply #'change-class mixin class :allow-other-keys t args)))

(defun remove-mixin-class (mixin class)
  (cond
    ((typep mixin 'mixin)
     (let ((class (ensure-mixin-class
                   (make-mixin-class-list
                    (remove (find-mixin-class class)
                            (classes (find-mixin-class (class-of mixin))))))))
       (change-class mixin class)))
    ((typep mixin class)
     (error "Cannot remove the only class of a mixin."))
    (t (error "Mixin does not contain class ~s." class))))
