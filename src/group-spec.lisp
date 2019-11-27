(in-package #:pyx)

(defclass group-spec ()
  ((%name :reader name
          :initarg :name)
   (%draw-order :reader draw-order
                :initarg :draw-order
                :initform 0)))

(u:define-printer (group-spec stream)
  (format stream "~s" (name group-spec)))

(defmethod initialize-instance :after ((instance group-spec) &key)
  (with-slots (%name %draw-order) instance
    (unless (typep %draw-order '(integer 0))
      (error "Group ~s must have a draw order greater than or equal to 0."
             %name))))

(defmacro define-groups (options &body body)
  (declare (ignore options))
  `(progn
     (unless (meta :groups)
       (setf (meta :groups) (u:dict #'eq)))
     (setf ,@(mapcan
              (lambda (x)
                (destructuring-bind (name . args) x
                  `((meta :groups ',name)
                    (apply #'make-instance 'group-spec
                           :name ',name
                           ,(when args
                              `(list ,@args))))))
              body))))

(define-groups ()
  (default :draw-order 0))
