(in-package #:pyx)

(defclass prefab-node ()
  ((%path :reader path
          :initarg :path)
   (%parent :reader parent
            :initarg :parent)
   (%template :reader template
              :initarg :template)
   (%component-types :reader component-types
                     :initform (u:dict #'eq :resolved nil))
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

(defun parse-node-template (path options)
  (destructuring-bind (&key template &allow-other-keys) options
    (etypecase template
      (null nil)
      (symbol (find-prefab-node-prototype template path))
      (cons (find-prefab-node template path)))))

(defun make-prefab-node (prefab path options args)
  (let ((node (make-instance 'prefab-node
                             :path path
                             :parent (u:href (nodes prefab) (butlast path))
                             :template (parse-node-template path options))))
    (with-slots (%component-args) node
      (setf (u:href %component-args :self) (u:plist->hash args :test #'eq))
      node)))

(defmethod resolve-prefab-node-args ((node prefab-node) template)
  (with-slots (%template %component-types %component-args) node
    (let ((types (compute-component-order
                  (u:href (component-types %template) :resolved)))
          (args (u:hash-merge (u:href (component-args %template) :resolved)
                              (u:href %component-args :self))))
      (setf (u:href %component-types :resolved) types
            (u:href %component-args :resolved) args))))

(defmethod resolve-prefab-node-args ((node prefab-node) (template null))
  (with-slots (%component-types %component-args) node
    (let ((types (compute-component-order nil))
          (args (u:href %component-args :self)))
      (setf (u:href %component-types :resolved) types
            (u:href %component-args :resolved) args))))
