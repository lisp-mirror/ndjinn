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
          (types (list* 'identify 'node 'xform types)))
      (u:do-hash (type order (meta :component-order))
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
  (compute-component-order (u:hash-keys (meta :component-order))))

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
     (defmethod has-component-p ((type (eql ',type)) entity)
       (typep entity ',type))
     (unless (meta :component-order)
       (setf (meta :component-order) (u:dict #'eq)))
     (setf (meta :component-order ',type)
           (list :before ',(a:ensure-list before)
                 :after ',(a:ensure-list after)))))

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
  (if (member component-type '(node xform))
      (error "The ~s component is immutable and cannot be removed."
             component-type)
      (register-entity-flow-event
       :component-remove
       (lambda ()
         (let ((entity (remove-mixin-class entity component-type)))
           (on-component-removed entity component-type))))))
