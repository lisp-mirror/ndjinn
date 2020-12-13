(in-package #:ndjinn)

;;; Begin parsing the raw data of the prefab. If any part of the parsing fails,
;;; clean up by removing the prefab entry from the metadata store.

(defun parse-prefab (prefab)
  (let (success-p)
    (unwind-protect
         (progn
           (populate-explicit-prefab-nodes prefab)
           (parse-prefab-node-templates prefab)
           (populate-implicit-prefab-nodes prefab)
           (record-prefab-dependencies prefab)
           (resolve-prefab-component-types prefab)
           (resolve-prefab-component-args prefab)
           (build-prefab-factory prefab)
           (setf success-p t))
      (unless success-p
        (remhash (name prefab) =meta/prefabs=)))))

;;; Populate the prefab object with all of its explicitly defined nodes by
;;; recursively parsing the raw data. The resulting nodes are not fully realized
;;; yet. NOTE: This does not populate the entire node tree -- only the explicit
;;; nodes of the raw data for this prefab definition. When we later fully
;;; realize these explicit nodes, we may find out that some use other prefab
;;; nodes as templates, and so we must then populate the implicit nodes that are
;;; children of those templates.

(defun populate-explicit-prefab-nodes (prefab)
  (with-slots (%name %data %nodes %root) prefab
    (labels ((populate (data &optional parent)
               (destructuring-bind (name options args children) data
                 (let* ((path `(,@parent ,name))
                        (parent (u:href %nodes (butlast path)))
                        (args (u:plist->hash args :test #'eq))
                        (node (make-instance 'prefab-node
                                             :prefab prefab
                                             :path path
                                             :parent parent
                                             :options options)))
                   (setf (u:href (component-args node) :self) args
                         (u:href %nodes path) node)
                   (map nil (lambda (x) (populate x path)) children)))))
      (map nil #'populate %data)
      (setf %root (u:href %nodes (list %name))))))

;;; Parse prefab node templates. This iterates through all explicit nodes and
;;; assigns the node template to its template slot if one is specified in its
;;; options.

(defun parse-prefab-node-templates (prefab)
  (u:do-hash-values (node (nodes prefab))
    (with-slots (%path %options %template) node
      (u:when-let ((spec (getf %options :template)))
        (setf %template (find-prefab-node-template spec %path))))))

;;; Populate the prefab with the implicit nodes. Prefab nodes before this step
;;; that have a template only have a node representing that template. This will
;;; bring in all of the children nodes of that template node into the correct
;;; paths.

(defun populate-implicit-prefab-nodes (prefab)
  (let ((nodes (nodes prefab)))
    (flet ((populate (parent template)
             (let ((template-path (path template)))
               (u:do-hash (path node (nodes
                                      (u:href =meta/prefabs=
                                              (car template-path))))
                 (let ((target-path (append (path parent)
                                            (nthcdr (length template-path)
                                                    path))))
                   (u:if-let ((target (u:href nodes target-path)))
                     (setf (template target) node)
                     (let ((parent (u:href (nodes prefab)
                                           (butlast target-path))))
                       (setf (u:href (nodes prefab) target-path)
                             (make-instance 'prefab-node
                                            :prefab prefab
                                            :path target-path
                                            :parent parent
                                            :template node)))))))))
      (u:do-hash-values (node (u:copy-hash-table nodes))
        (u:when-let ((template (template node)))
          (populate node template))))))

;;; Record the dependencies of a prefab. This iterates over all nodes and if it
;;; uses a template, adds the current prefab name to the template's slave list,
;;; and also adds the template's prefab name to the current prefab's master
;;; list. Additionally, we track any dependencies that have been removed since
;;; the last definition compile, and removes from the slaves and masters
;;; accordingly. This gives us information needed to be able to live recompile a
;;; prefab and have the changes apply transitively to its children.

(defgeneric record-prefab-template-dependency (prefab template)
  (:method ((prefab prefab) (template null)))
  (:method ((prefab prefab) (template prefab-node))
    (let ((template-prefab (prefab template)))
      (pushnew (name prefab) (slaves template-prefab))
      (pushnew (name template-prefab) (masters prefab)))))

(defun record-prefab-dependencies (prefab)
  (let ((old-masters (masters prefab)))
    (setf (masters prefab) nil)
    (u:do-hash-values (node (nodes prefab))
      (record-prefab-template-dependency prefab (template node)))
    (dolist (master-spec old-masters)
      (let ((master (u:href =meta/prefabs= master-spec)))
        (unless (find master-spec (masters prefab))
          (u:deletef (slaves master) (name prefab)))))))

;;; Resolve the component types of all nodes. This iterates over all nodes and
;;; combines the specified types of that node with the resolved types of its
;;; template. Node types are specified with the node options :ADD and :REMOVE,
;;; which is a list of symbols denoting the names of components. The resolution
;;; process is as follows: First, get all of the final resolved component types
;;; of the node's template if it has one. Then, delete the component types given
;;; by :REMOVE. Then, add the component types given by :ADD. Then, add in any
;;; required types that are missing that all nodes must possess. Finally, sort
;;; the resulting list of types topologically in the order the game engine uses
;;; them.

(defun resolve-prefab-component-types (prefab)
  (u:do-hash-values (node (nodes prefab))
    (with-slots (%options %template %component-types) node
      (let* ((order =meta/component-order=)
             (types (when %template
                      (copy-seq
                       (u:href (component-types %template) :resolved)))))
        (destructuring-bind (&key add remove &allow-other-keys) %options
          (dolist (type remove)
            (unless (u:href order type)
              (error "Cannot remove component from prefab ~s: component ~s is ~
                      not a defined component."
                     (name prefab)
                     type))
            (u:deletef types type))
          (dolist (type add)
            (unless (u:href order type)
              (error "Cannot add component to prefab ~s: ~s is not a defined ~
                      component."
                     (name prefab)
                     type))
            (pushnew type types))
          (let ((types (compute-component-order types)))
            (setf (u:href %component-types :self) add
                  (u:href %component-types :removed) remove
                  (u:href %component-types :resolved) types)))))))

;;; Resolve the component arguments of all nodes. This iterates over all nodes
;;; and combines the specified arguments of that node with the resolved
;;; arguments of its template. Also, checks are performed to ensure that any
;;; specified arguments conform to the component types of that node.

(defun resolve-prefab-component-args (prefab)
  (u:do-hash (path node (nodes prefab))
    (with-slots (%template %component-types %component-args) node
      (let ((removed (mapcan #'compute-component-initargs
                             (u:href %component-types :removed)))
            (valid (mapcan #'compute-component-initargs
                           (u:href %component-types :resolved)))
            (args (if %template
                      (u:hash-merge (u:href (component-args %template)
                                            :resolved)
                                    (u:href %component-args :self))
                      (u:href %component-args :self))))
        (u:do-hash-keys (arg args)
          (when (find arg removed)
            (remhash arg args)))
        (u:do-hash-keys (arg args)
          (unless (find arg valid)
            (error "Component argument ~s is invalid for node ~{~a~^/~}."
                   arg path)))
        (setf (u:href %component-args :resolved) args)))))

;;; Pre-process the prefab definition body to control its evaluation. This wraps
;;; raw s-expressions in LIST, and wraps component arguments in a LAMBDA to be
;;; lazily evaluated later. It also wraps the special @ macro around the data to
;;; allow for concise referencing of the final constructed entity of an
;;; arbitrary prefab node, for when a component argument needs such a reference.
;;; NOTE: This occurs at compile-time, so we must not do anything that requires
;;; a prefab instance - only process the data that is to be later parsed when
;;; creating a prefab.

(defmacro preprocess-prefab-data (name options data)
  (labels ((process (data)
             (destructuring-bind (name options args children)
                 (loop :with ((name . options) . rest) = data
                       :for body :on rest :by #'cddr
                       :for (k v) = body
                       :while (keywordp (first body))
                       :collect k :into args
                       :collect v :into args
                       :finally (return (list name options args body)))
               `(list ',name
                      ,(when options `',options)
                      ,(when args
                         `(list
                           ,@(loop :for (k v) :on args :by #'cddr
                                   :collect k
                                   :collect `(lambda () ,v))))
                      ,(when children
                         `(list ,@(mapcar #'process children)))))))
    `(macrolet ((,(u:symbolicate "@") (&rest reference-spec)
                  `(make-prefab-reference ',reference-spec)))
       (list ,@(mapcar #'process (list (list* (list* name options) data)))))))
