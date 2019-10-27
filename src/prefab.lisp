(in-package #:pyx)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%root :reader root)
   (%nodes :reader nodes
           :initform (u:dict #'equal))
   (%func :reader func)))

(u:define-printer (prefab stream)
  (format stream "~s" (name prefab)))

(defun find-prefab (name)
  (or (meta :prefabs name)
      (error "Prefab ~s not found." name)))

(defun reset-prefab (prefab)
  (with-slots (%root %nodes) prefab
    (setf %root nil)
    (clrhash %nodes)
    prefab))

(defun make/reset-prefab (name)
  (a:if-let ((prefab (meta :prefabs name)))
    (reset-prefab prefab)
    (make-instance 'prefab :name name)))

(defun resolve-prefab-nodes (prefab)
  (u:do-hash-values (node (nodes prefab))
    (resolve-prefab-node-args node (template node))))

(defun parse-prefab (prefab entities)
  (let (success-p)
    (unwind-protect
         (with-slots (%name %root %nodes %func) prefab
           (resolve-prefab-nodes prefab)
           (setf %root (u:href %nodes (list %name))
                 %func (make-prefab-factory prefab entities)
                 success-p t))
      (when success-p
        (setf (meta :prefabs (name prefab)) prefab)))))

(defun parse-prefab/data (prefab data &optional parent)
  (flet ((thunk-args (args)
           (loop :for (k v) :on args :by #'cddr
                 :collect k
                 :collect `(lambda () ,v))))
    (let ((data (parse-prefab-node-data data)))
      (destructuring-bind (name options args children) data
        (let ((path `(,@parent ,name)))
          `((u:href (nodes ,prefab) ',path)
            (make-prefab-node ,prefab
                              ',path
                              ',options
                              (list ,@(thunk-args args)))
            ,@(mapcan
               (lambda (x)
                 (parse-prefab/data prefab x path))
               children)))))))

(defun resolve-prefab-entity-components (entity-table prefab-parent node)
  (let ((args (u:dict #'eq)))
    (with-slots (%parent %component-types %component-args) node
      (u:do-hash (k v (u:href %component-args :resolved))
        (setf (u:href args k) (funcall v)))
      (setf (u:href args :node/parent)
            (if %parent
                (u:href entity-table (path %parent))
                prefab-parent))
      (values (u:href %component-types :resolved)
              args))))

(defun make-prefab-factory (prefab entities)
  (with-slots (%name %nodes %root) prefab
    (lambda (&key parent)
      (let ((parent (or parent (node-tree *state*))))
        (u:do-hash (path node %nodes)
          (u:mvlet* ((types args (resolve-prefab-entity-components
                                  entities parent node))
                     (entity (%make-entity types args)))
            (setf (u:href entities path) entity)))
        (let ((root (u:href entities (path %root))))
          (push root (u:href (prefabs (database *state*)) %name))
          (setf (identify/prefab root) %name)
          root)))))

(defun load-prefab (name &key parent)
  (register-entity-flow-event
   :prefab-create
   (lambda ()
     (funcall (func (find-prefab name)) :parent parent))))

(defun deregister-prefab-entity (entity)
  (a:when-let* ((prefab (identify/prefab entity))
                (table (prefabs (database *state*))))
    (a:deletef (u:href table prefab) entity)
    (unless (u:href table prefab)
      (remhash prefab table))))

(defmacro wrap-prefab-reference-lookup (prefab-name entities &body body)
  `(flet ((%ref (path)
            (when path
              (u:href ,entities path))))
     (macrolet ((,(a:symbolicate "@") (&rest path/query)
                  (let ((path (cons ',prefab-name path/query))
                        (query (car (last path/query))))
                    (if (keywordp query)
                        `(%entity-query (%ref ',(butlast path)) ,query)
                        `(%ref ',path)))))
       (%ref nil)
       ,@body)))

(defmacro define-prefab (name options &body body)
  (a:with-gensyms (prefab entities)
    `(let ((,entities (u:dict #'equal)))
       (wrap-prefab-reference-lookup ,name ,entities
         (unless (meta :prefabs)
           (setf (meta :prefabs) (u:dict #'eq)))
         (let ((,prefab (make/reset-prefab ',name)))
           (setf ,@(mapcan
                    (lambda (x) (parse-prefab/data prefab x))
                    (list (list* name options body))))
           (parse-prefab ,prefab ,entities)
           (update-prefab-entities ,prefab))))))

(defun update-prefab-entities (prefab)
  (enqueue :recompile (list :prefab (name prefab))))

(defun recompile-prefab (name)
  (dolist (entity (u:href (prefabs (database *state*)) name))
    (let ((parent (node/parent entity)))
      (deregister-prefab-entity entity)
      (load-prefab name :parent parent))))

;;; Prefab nodes

(defclass prefab-node ()
  ((%path :reader path
          :initarg :path)
   (%parent :reader parent
            :initarg :parent)
   (%template :reader template
              :initarg :template)
   (%component-types :reader component-types
                     :initform (u:dict #'eq :self nil
                                            :remove nil
                                            :resolved nil))
   (%component-args :reader component-args
                    :initform (make-nested-dict #'eq :self :resolved))))

(u:define-printer (prefab-node stream)
  (format stream "~{~a~^/~}" (path prefab-node)))

(defun find-prefab-node (template-spec path)
  (flet ((not-found ()
           (error "Template node ~{~a~^/~} not found for prefab node ~{~a~^/~}."
                  template-spec path)))
    (a:if-let ((prefab (meta :prefabs (first template-spec))))
      (or (u:href (nodes prefab) template-spec)
          (not-found))
      (not-found))))

(defun find-prefab-node-prototype (template-spec path)
  (or (meta :prototypes template-spec)
      (error "Prototype ~s not found for prefab node ~{~a~^/~}."
             template-spec path)))

(defun parse-prefab-node-data (data)
  (loop :with (name options . rest) = data
        :for body :on rest :by #'cddr
        :while (keywordp (first body))
        :for (k v) = body
        :collect k :into args
        :collect v :into args
        :finally (return (list name options args body))))

(defun parse-node-template (path template-spec)
  (etypecase template-spec
    (null nil)
    (symbol (find-prefab-node-prototype template-spec path))
    (cons (find-prefab-node template-spec path))))

(defun make-prefab-node (prefab path options args)
  (destructuring-bind (&key template add remove &allow-other-keys) options
    (let ((node (make-instance 'prefab-node
                               :path path
                               :template (parse-node-template path template)
                               :parent (u:href (nodes prefab) (butlast path)))))
      (with-slots (%component-types %component-args) node
        (setf (u:href %component-types :self) (sort add #'string<)
              (u:href %component-types :remove) (sort remove #'string<)
              (u:href %component-args :self) (u:plist->hash args :test #'eq))
        node))))

(defun resolve-prefab-node-types (node)
  (with-slots (%template %component-types) node
    (let* ((types (when %template
                    (u:href (component-types %template) :resolved)))
           (filtered (set-difference types (u:href %component-types :remove))))
      (compute-component-order
       (sort
        (remove-duplicates
         (append (u:href %component-types :self) filtered))
        #'string<)))))

(defmethod resolve-prefab-node-args ((node prefab-node) template)
  (with-slots (%template %component-types %component-args) node
    (let ((types (resolve-prefab-node-types node))
          (removed-args (mapcan #'compute-component-args
                                (u:href %component-types :remove)))
          (args (u:hash-merge (u:href (component-args %template) :resolved)
                              (u:href %component-args :self))))
      (u:do-hash-keys (k args)
        (when (member k removed-args)
          (remhash k args)))
      (setf (u:href %component-types :resolved) types
            (u:href %component-args :resolved) args))))

(defmethod resolve-prefab-node-args ((node prefab-node) (template null))
  (with-slots (%component-types %component-args) node
    (let ((types (resolve-prefab-node-types node))
          (args (u:href %component-args :self)))
      (setf (u:href %component-types :resolved) types
            (u:href %component-args :resolved) args))))
