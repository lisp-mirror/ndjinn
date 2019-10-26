(in-package #:pyx)

(defun compute-component-order (types)
  (flet ((dag-p (graph)
           (unless (or (cl-graph:find-edge-if
                        graph #'cl-graph:undirected-edge-p)
                       (cl-graph:find-vertex-if
                        graph
                        (lambda (x) (cl-graph:in-cycle-p graph x)))))))
    (let ((graph (cl-graph:make-graph 'cl-graph:graph-container
                                      :default-edge-type :directed))
          (types (append (meta :components :static) types)))
      (u:do-hash (type order (meta :components :order))
        (cl-graph:add-vertex graph type)
        (destructuring-bind (&key before after) order
          (dolist (x before)
            (cl-graph:add-edge-between-vertexes graph type x))
          (dolist (x after)
            (cl-graph:add-edge-between-vertexes graph x type))))
      (unless (dag-p graph)
        "Component order cannot be computed because the graph is not a DAG.")
      (remove-if-not
       (lambda (x) (find x types))
       (mapcar #'cl-graph:element (cl-graph:topological-sort graph))))))

(defun compute-all-components-order ()
  (compute-component-order (u:hash-keys (meta :components :order))))

(defun compute-component-args (component-type)
  (let* ((class (c2mop:ensure-finalized (find-class component-type)))
         (class-args (a:mappend #'c2mop:slot-definition-initargs
                                (c2mop:class-slots class)))
         (instance-lambda-list (c2mop:method-lambda-list
                                (first
                                 (c2mop:compute-applicable-methods-using-classes
                                  #'reinitialize-instance
                                  (list class)))))
         (instance-args (mapcar
                         (lambda (x)
                           (u:make-keyword (car (a:ensure-list x))))
                         (rest (member '&key instance-lambda-list)))))
    (union class-args instance-args)))

(defmacro define-component (type (&key before after) &body body)
  `(u:eval-always
     (defclass ,type ()
       ,(loop :for (key value) :on (car body) :by #'cddr
              :for accessor = (a:symbolicate type '#:/ key)
              :for slot = (a:symbolicate '#:% accessor)
              :for initarg = (a:make-keyword accessor)
              :collect `(,slot :accessor ,accessor
                               :initarg ,initarg
                               :initform ,value)))
     (unless (meta :components)
       (setf (meta :components) (u:dict #'eq :order (u:dict #'eq) :static nil)))
     (defmethod has-component-p ((type (eql ',type)) entity)
       (typep entity ',type))
     (setf (meta :components :order ',type)
           (list :before ',(a:ensure-list before)
                 :after ',(a:ensure-list after)))))

(defmacro define-component/static (type (&key before after) &body body)
  `(progn
     (define-component ,type (:before ,before :after ,after) ,@body)
     (pushnew ',type (meta :components :static))))

(defgeneric on-component-added (entity component-type)
  (:method (entity component-type)))

(defgeneric on-component-removed (entity component-type)
  (:method (entity component-type)))

(defun add-component (entity component-type &rest args)
  (register-entity-flow-event
   :component-add
   (lambda ()
     (let ((entity (apply #'add-mixin-class entity component-type args)))
       (on-component-added entity component-type)))))

(defun remove-component (entity component-type)
  (if (member component-type (meta :components :static))
      (error "Component ~s is static and cannot be removed." component-type)
      (register-entity-flow-event
       :component-remove
       (lambda ()
         (let ((entity (remove-mixin-class entity component-type)))
           (on-component-removed entity component-type))))))

(defun remove-components (entity)
  (dolist (component (get-mixin-class-names entity))
    (unless (member component (meta :components :static))
      (remove-component entity component))))
