(in-package :pyx)

(defclass pipeline-spec ()
  ((%name :reader name
          :initarg :name)
   (%pass-order :reader pass-order
                :initarg :pass-order)
   (%pass-options :reader pass-options
                  :initarg :pass-options)
   (%draw-order :reader draw-order
                :initarg :draw-order)
   (%scenes :accessor scenes
            :initform nil)))

(u:define-printer (pipeline-spec stream)
  (format stream "~s" (name pipeline-spec)))

(define-event-handler :recompile :pipeline recompile-pipeline)

(defun find-pipeline-spec (name)
  (or (meta :pipelines name)
      (error "Pipeline ~s is not defined." name)))

(defun update-pipeline-spec (name pass-order pass-options draw-order)
  (with-slots (%pass-order %pass-options %draw-order %scenes)
      (meta :pipelines name)
    (setf %pass-order pass-order
          %pass-options pass-options
          %draw-order draw-order)
    (dolist (scene %scenes)
      (unless (eq name (name (pipeline (meta :scenes scene))))
        (a:deletef %scenes scene)))
    (enqueue :recompile (list :pipeline name))))

(defun make-pipeline-spec (name pass-order pass-options draw-order)
  (let ((pipeline-spec (make-instance 'pipeline-spec :name name)))
    (setf (meta :pipelines name) pipeline-spec)
    (update-pipeline-spec name pass-order pass-options draw-order)
    pipeline-spec))

(u:eval-always
  (defun preprocess-pipeline-passes (passes)
    (when passes
      `(list
        ,@(mapcar
           (lambda (x)
             (if (listp x)
                 `(list ',(car x) ,@(rest x))
                 `(list ',x)))
           passes)))))

(defun parse-pipeline-passes (passes)
  (loop :with table = (u:dict #'eq)
        :for (name . options) :in passes
        :collect name :into order
        :do (destructuring-bind (&key clear-color clear-buffers) options
              (let* ((clear-color (or clear-color (v4:vec 0f0 0f0 0f0 1f0)))
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

(defun recompile-pipeline (name)
  (let ((pipeline (meta :pipelines name))
        (scene (current-scene *state*)))
    (dolist (pass (pass-order pipeline))
      (sort-draw-order scene pass))))

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
                 (make-pipeline-spec ',name
                                     ,pass-order
                                     ,pass-options
                                     ,draw-order-table))))))))

(define-pipeline :default ()
  (:passes (:default)
   :draw-order (:default)))
