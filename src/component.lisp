(in-package #:%pyx.entity)

(glob:define-global-var =component-order= (u:dict #'eq))
(glob:define-global-var =component-initargs= (u:dict #'eq))
(glob:define-global-var =static-components= nil)

(defun compute-component-order (types)
  (flet ((dag-p (graph)
           (unless (or (gph:find-edge-if graph #'gph:undirected-edge-p)
                       (gph:find-vertex-if
                        graph
                        (lambda (x) (gph:in-cycle-p graph x)))))))
    (let ((graph (gph:make-graph 'gph:graph-container
                                 :default-edge-type :directed))
          (types (append =static-components= types)))
      (u:do-hash (type order =component-order=)
        (gph:add-vertex graph type)
        (destructuring-bind (&key before after) order
          (dolist (x before)
            (gph:add-edge-between-vertexes graph type x))
          (dolist (x after)
            (gph:add-edge-between-vertexes graph x type))))
      (unless (dag-p graph)
        "Component order cannot be computed because the graph is not a DAG.")
      (remove-if-not
       (lambda (x) (find x types))
       (mapcar #'gph:element (gph:topological-sort graph))))))

(defun compute-all-components-order ()
  (compute-component-order (u:hash-keys =component-order=)))

(defun compute-component-accessors (type)
  (labels ((get-all-direct-slots (class)
             (append (c2mop:class-direct-slots class)
                     (a:mappend #'get-all-direct-slots
                                (c2mop:class-direct-superclasses class)))))
    (let* ((class (c2mop:ensure-finalized (find-class type)))
           (slots (get-all-direct-slots class)))
      (remove-duplicates
       (remove-if
        (lambda (x)
          (or (null x)
              (and (not (eq (symbol-package x) *package*))
                   (not (eq (nth-value 1 (find-symbol (symbol-name x)
                                                      (symbol-package x)))
                            :external)))))
        (a:mappend (lambda (x)
                     (append (c2mop:slot-definition-readers x)
                             (mapcar #'second
                                     (c2mop:slot-definition-writers x))))
                   slots))))))

(defun compute-component-initargs (type)
  (let* ((class (c2mop:ensure-finalized (find-class type)))
         (class-args (a:mappend #'c2mop:slot-definition-initargs
                                (c2mop:class-slots class)))
         (instance-lambda-list (c2mop:method-lambda-list
                                (first
                                 (c2mop:compute-applicable-methods-using-classes
                                  #'reinitialize-instance
                                  (list class)))))
         (instance-args (mapcar
                         (lambda (x)
                           (a:make-keyword (car (a:ensure-list x))))
                         (rest (member '&key instance-lambda-list)))))
    (union class-args instance-args)))

(defun track-component-initargs (type slots)
  (flet ((clear-type ()
           (u:do-hash (k v =component-initargs=)
             (when (eq type v)
               (remhash k =component-initargs=)))))
    (clear-type)
    (dolist (slot slots)
      (loop :for (k v) :on (cdr slot) :by #'cddr
            :when (eq k :initarg)
              :do (a:if-let ((cached-type (u:href =component-initargs= v)))
                    (unwind-protect
                         (error "Component initarg ~s of component ~s is ~
                                 already in use by component ~s."
                                v type cached-type)
                      (clear-type))
                    (setf (u:href =component-initargs= v) type))))))

;;; Public API

(defmacro define-component (name super-classes &body slots/options)
  (destructuring-bind (&optional slots . options) slots/options
    (let ((sorting (cdr (find :sorting options :key #'car)))
          (static (cadr (find :static options :key #'car)))
          (class-options (remove-if
                          (lambda (x) (find x '(:sorting :static)))
                          options
                          :key #'car)))
      (destructuring-bind (&key before after) sorting
        (declare (ignorable before after))
        `(u:eval-always
           (track-component-initargs ',name ',slots)
           (defclass ,name ,super-classes ,slots ,@class-options)
           (setf (u:href =component-order= ',name)
                 '(:before ,(a:ensure-list before)
                   :after ,(a:ensure-list after)))
           (unless (typep ',static 'boolean)
             (error ":STATIC must be either T or NIL."))
           ,@(when (eq static t)
               `((pushnew ',name =static-components=))))))))
