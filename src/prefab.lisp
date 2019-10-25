(in-package #:pyx)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%root :reader root)
   (%nodes :reader nodes
           :initform (u:dict #'equal))
   (%func :reader func)))

(defun find-prefab (name)
  (a:if-let ((prefab (meta :prefabs name)))
    prefab
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
           (setf %root (u:href %nodes (list %name)))
           (resolve-prefab-nodes prefab)
           (setf %func (make-prefab-factory prefab entities)
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

(defun load-prefab (name)
  (register-entity-flow-event
   :prefab-create
   (lambda ()
     (funcall (func (find-prefab name))))))

(defun deregister-prefab-entity (entity)
  (a:when-let* ((prefab (identify/prefab entity))
                (table (prefabs (database *state*))))
    (a:deletef (u:href table prefab) entity)
    (unless (u:href table prefab)
      (remhash prefab table))))

(defmacro wrap-prefab-reference-lookup (prefab-name entities &body body)
  `(flet ((ref (path)
            (when path
              (u:href ,entities path))))
     (macrolet ((,(a:symbolicate "@") (&rest path/query)
                  (let* ((path (cons ',prefab-name path/query))
                         (query (car (last path/query))))
                    (if (keywordp query)
                        `(%entity-query (ref ',(butlast path)) ,query)
                        `(ref ',path)))))
       (ref nil)
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
           (parse-prefab ,prefab ,entities))))))
