(in-package #:net.mfiano.lisp.pyx)

(glob:define-global-var =component-type-order= (u:dict #'eq))
(glob:define-global-var =component-initargs= (u:dict #'eq))
(glob:define-global-var =static-components= nil)

(defun sort-component-types (order-table types)
  (flet ((dag-p (graph)
           (not (or (gph:find-edge-if graph #'gph:undirected-edge-p)
                    (gph:find-vertex-if
                     graph
                     (lambda (x) (gph:in-cycle-p graph x)))))))
    (let ((graph (gph:make-graph 'gph:graph-container
                                 :default-edge-type :directed)))
      (u:do-hash (type order order-table)
        (gph:add-vertex graph type)
        (destructuring-bind (&key before after) order
          (dolist (x before)
            (gph:add-edge-between-vertexes graph type x))
          (dolist (x after)
            (gph:add-edge-between-vertexes graph x type))))
      (unless (dag-p graph)
        (error "Component order cannot be computed because the graph is not a ~
                DAG."))
      (remove-if-not
       (lambda (x) (find x types))
       (mapcar #'gph:element (gph:topological-sort graph))))))

(defun compute-component-type-order (types)
  (sort-component-types =component-type-order=
                        (append =static-components= types)))

(defun compute-total-component-type-order ()
  (compute-component-type-order (u:hash-keys =component-type-order=)))

(defun compute-component-accessors (type)
  (labels ((get-all-direct-slots (class)
             (append (c2mop:class-direct-slots class)
                     (u:mappend #'get-all-direct-slots
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
        (u:mappend (lambda (x)
                     (append (c2mop:slot-definition-readers x)
                             (mapcar #'second
                                     (c2mop:slot-definition-writers x))))
                   slots))))))

(defun compute-component-initargs (type)
  (let ((methods nil)
        (class (c2mop:ensure-finalized (find-class type))))
    (push (find-method #'initialize-instance '(:before) (list class) nil)
          methods)
    (push (find-method #'initialize-instance '(:after) (list class) nil)
          methods)
    (push (find-method #'initialize-instance '(:around) (list class) nil)
          methods)
    (push (find-method #'reinitialize-instance '(:before) (list class) nil)
          methods)
    (push (find-method #'reinitialize-instance '(:after) (list class) nil)
          methods)
    (push (find-method #'reinitialize-instance '(:around) (list class) nil)
          methods)
    (push (find-method #'shared-initialize '(:before) (list class t) nil)
          methods)
    (push (find-method #'shared-initialize '(:after) (list class t) nil)
          methods)
    (push (find-method #'shared-initialize '(:around) (list class t) nil)
          methods)
    (remove-duplicates
     (union (u:mappend #'c2mop:slot-definition-initargs
                       (c2mop:class-slots class))
            (mapcan
             (lambda (x)
               (mapcar #'u:make-keyword
                       (rest (member '&key (c2mop:method-lambda-list x)))))
             (remove nil methods))))))

(defun track-component-initargs (type slots)
  (flet ((clear-type ()
           (u:do-hash (k v =component-initargs=)
             (when (eq type v)
               (remhash k =component-initargs=)))))
    (clear-type)
    (dolist (slot slots)
      (loop :for (k v) :on (cdr slot) :by #'cddr
            :when (eq k :initarg)
              :do (u:if-let ((cached-type (u:href =component-initargs= v)))
                    (unwind-protect
                         (error "Component initarg ~s of component ~s is ~
                                 already in use by component ~s."
                                v type cached-type)
                      (clear-type))
                    (setf (u:href =component-initargs= v) type))))))

(defmacro define-component (name super-classes &body slots/options)
  (destructuring-bind (&optional slots . options) slots/options
    (let ((type-order (cdr (find :type-order options :key #'car)))
          (static (cadr (find :static options :key #'car)))
          (class-options (remove-if
                          (lambda (x) (find x '(:type-order :static)))
                          options
                          :key #'car)))
      (destructuring-bind (&key before after) type-order
        (declare (ignorable before after))
        `(u:eval-always
           (track-component-initargs ',name ',slots)
           (defclass ,name ,super-classes ,slots ,@class-options)
           (setf (u:href =component-type-order= ',name)
                 '(:before ,(u:ensure-list before)
                   :after ,(u:ensure-list after)))
           ,@(if (eq static t)
                 `((pushnew ',name =static-components=))
                 `((u:deletef =static-components= ',name)))
           ,@(unless (typep static 'boolean)
               `((error ":STATIC must be either T or NIL."))))))))
