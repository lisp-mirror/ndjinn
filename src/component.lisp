(in-package #:pyx)

(defvar *component-order* (u:dict #'eq))

(defun compute-component-order (types)
  (flet ((dag-p (graph)
           (unless (or (cl-graph:find-edge-if
                        graph #'cl-graph:undirected-edge-p)
                       (cl-graph:find-vertex-if
                        graph
                        (lambda (x) (cl-graph:in-cycle-p graph x)))))))
    (let ((graph (cl-graph:make-graph 'cl-graph:graph-container
                                      :default-edge-type :directed))
          (types (list* 'node 'xform types)))
      (u:do-hash (type order *component-order*)
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
  (compute-component-order (u:hash-keys *component-order*)))

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
     (setf (u:href *component-order* ',type)
           (list :before ',(a:ensure-list before)
                 :after ',(a:ensure-list after)))))

(defgeneric on-component-added (component entity)
  (:method (component entity)))

(defgeneric on-component-removed (component entity)
  (:method (component entity)))

(defun add-component (entity component &rest args)
  (let ((entity (apply #'add-mixin-class entity component args)))
    (on-component-added component entity)))

(defun remove-component (entity component)
  (if (member component '(node xform))
      (error "The ~s component is immutable and cannot be removed."
             component)
      (let ((entity (remove-mixin-class entity component)))
        (on-component-removed component entity))))
