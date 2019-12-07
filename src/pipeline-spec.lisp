(in-package :pyx)

(defclass pipeline-spec ()
  ((%name :reader name
          :initarg :name)
   (%passes :reader passes
            :initarg :passes)
   (%order :reader order
           :initarg :order)))

(defun find-pipeline-spec (name)
  (or (meta :pipelines name)
      (error "Pipeline ~s is not defined." name)))

(defun update-pipeline-spec (name passes order)
  (with-slots (%passes %order) (meta :pipelines name)
    (setf %passes passes
          %order order)))

(defun make-pipeline-order-table (order)
  (loop :with table = (u:dict #'eq :default 0)
        :for item :in order
        :for i :from 0
        :do (setf (u:href table item) i)
        :finally (return table)))

(defmacro define-pipeline (name options &body body)
  (declare (ignore options))
  (a:with-gensyms (order-table)
    (destructuring-bind (&key (passes '(:default)) (order '(:default)))
        (car body)
      `(let ((,order-table (make-pipeline-order-table ',order)))
         (unless (meta :pipelines)
           (setf (meta :pipelines) (u:dict #'eq)))
         (if (meta :pipelines ',name)
             (update-pipeline-spec ',name ',passes ,order-table)
             (setf (meta :pipelines ',name)
                   (make-instance 'pipeline-spec
                                  :name ',name
                                  :passes ',passes
                                  :order ,order-table)))))))

(define-pipeline :default ()
  (:passes (:default)
   :order (:default)))
