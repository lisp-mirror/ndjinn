(in-package #:net.mfiano.lisp.pyx)

(defun sort-component-types (order-table types)
  (let ((graph (digraph:make-digraph)))
    (u:do-hash (type order order-table)
      (digraph:insert-vertex graph type)
      (destructuring-bind (&key before after) order
        (dolist (x before)
          (unless (digraph:contains-vertex-p graph x)
            (digraph:insert-vertex graph x))
          (digraph:insert-edge graph x type))
        (dolist (x after)
          (unless (digraph:contains-vertex-p graph x)
            (digraph:insert-vertex graph x))
          (digraph:insert-edge graph type x))))
    (remove-if-not
     (lambda (x) (find x types))
     (digraph:topological-sort graph))))

(defun compute-component-type-order (types)
  (sort-component-types (metadata-components-type-order =metadata=)
                        (append (metadata-components-static =metadata=) types)))

(defun compute-total-component-type-order ()
  (compute-component-type-order
   (u:hash-keys (metadata-components-type-order =metadata=))))

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
  (let ((initargs (metadata-components-initargs =metadata=)))
    (flet ((clear-type ()
             (u:do-hash (k v initargs)
               (when (eq type v)
                 (remhash k initargs)))))
      (clear-type)
      (dolist (slot slots)
        (loop :for (k v) :on (cdr slot) :by #'cddr
              :when (eq k :initarg)
                :do (u:if-let ((cached-type (u:href initargs v)))
                      (unwind-protect
                           (error "Component initarg ~s of component ~s is ~
                                 already in use by component ~s."
                                  v type cached-type)
                        (clear-type))
                      (setf (u:href initargs v) type)))))))

(defmacro define-component (name super-classes &body slots/options)
  (u:with-gensyms (func)
    (destructuring-bind (&optional slots . options) slots/options
      (let ((type-order (cdr (find :type-order options :key #'car)))
            (static (cadr (find :static options :key #'car)))
            (class-options (remove-if
                            (lambda (x) (find x '(:type-order :static)))
                            options
                            :key #'car)))
        (destructuring-bind (&key before after) type-order
          (declare (ignorable before after))
          `(progn
             (u:eval-always
               (unless =context=
                 (defclass ,name ,super-classes ,slots ,@class-options)))
             (when =context=
               (let ((,func (lambda ()
                              (defclass ,name ,super-classes
                                ,slots
                                ,@class-options))))
                 (enqueue :recompile (list :component (list ',name ,func)))))
             (track-component-initargs ',name ',slots)
             (setf (u:href (metadata-components-type-order =metadata=) ',name)
                   '(:before ,(u:ensure-list before)
                     :after ,(u:ensure-list after)))
             (compute-total-component-type-order)
             ,@(if (eq static t)
                   `((pushnew ',name (metadata-components-static =metadata=)))
                   `((u:deletef (metadata-components-static =metadata=)
                                ',name)))
             ,@(unless (typep static 'boolean)
                 `((error ":STATIC must be either T or NIL.")))))))))

(on-recompile :component data ()
  (destructuring-bind (name func) data
    (funcall func)
    (log:debug :pyx.live "Recompiled component: ~s" name)))
