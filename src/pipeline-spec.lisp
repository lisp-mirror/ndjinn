(in-package :pyx)

(defclass pipeline-spec ()
  ((%name :reader name
          :initarg :name)
   (%pass-order :reader pass-order
                :initarg :pass-order)
   (%pass-options :reader pass-options
                  :initarg :pass-options)
   (%draw-order :reader draw-order
                :initarg :draw-order)))

(u:define-printer (pipeline-spec stream)
  (format stream "~s" (name pipeline-spec)))

(defun find-pipeline-spec (name)
  (or (meta :pipelines name)
      (error "Pipeline ~s is not defined." name)))

(defun update-pipeline-spec (name pass-order pass-options draw-order)
  (with-slots (%pass-order %pass-options %draw-order) (meta :pipelines name)
    (setf %pass-order pass-order
          %pass-options pass-options
          %draw-order draw-order)))

(defun parse-pipeline-passes (passes)
  (loop :with table = (u:dict #'eq)
        :for (name options) :on passes :by #'cddr
        :collect name :into order
        :do (destructuring-bind (&key clear-color clear-buffers) options
              (let* ((clear-color (or clear-color (v4:vec 0 0 0 1)))
                     (clear-buffers (mapcar
                                     (lambda (x)
                                       (a:format-symbol :keyword "~a-BUFFER" x))
                                     (or clear-buffers '(:color :depth))))
                     (options (list :clear-color clear-color
                                    :clear-buffers clear-buffers)))
                (setf (u:href table name) options)))
        :finally (return (values order table))))

(defun make-pipeline-draw-order-table (order)
  (loop :with table = (u:dict #'eq :default 0)
        :for item :in order
        :for i :from 0
        :do (setf (u:href table item) i)
        :finally (return table)))

(u:eval-always
  (defun preprocess-pipeline-passes (passes)
    (when passes
      `(list
        ,@(loop :for (k v) :on passes :by #'cddr
                :collect k
                :collect `(list ,@v))))))

(defmacro define-pipeline (name options &body body)
  (declare (ignore options))
  (a:with-gensyms (pass-order pass-options draw-order-table)
    (destructuring-bind (&key (passes '(:default)) (draw-order '(:default)))
        (car body)
      (let ((passes (preprocess-pipeline-passes passes)))
        `(u:mvlet ((,pass-order ,pass-options (parse-pipeline-passes ,passes))
                   (,draw-order-table (make-pipeline-draw-order-table
                                       ',draw-order)))
           (progn
             (unless (meta :pipelines)
               (setf (meta :pipelines) (u:dict #'eq)))
             (if (meta :pipelines ',name)
                 (update-pipeline-spec ',name
                                       ,pass-order
                                       ,pass-options
                                       ,draw-order-table)
                 (setf (meta :pipelines ',name)
                       (make-instance 'pipeline-spec
                                      :name ',name
                                      :pass-order ,pass-order
                                      :pass-options ,pass-options
                                      :draw-order ,draw-order-table)))))))))

(define-pipeline :default ()
  (:passes (:default)
   :draw-order (:default)))
