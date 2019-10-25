(in-package #:pyx)

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
