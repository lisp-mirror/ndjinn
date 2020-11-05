(in-package #:net.mfiano.lisp.pyx)

(defun make-prefab-entity-skeleton (node)
  (with-slots (%component-types %component-args) node
    (let* ((types (u:href %component-types :resolved))
           (args (u:href %component-args :resolved))
           (entity (make-instance 'prefab-entity-skeleton
                                  :mixin-class (make-entity-class types)
                                  :types types
                                  :args args)))
      (dolist (slot (get-entity-slots types))
        (let ((key (u:make-keyword (string-left-trim "%" (symbol-name slot)))))
          (setf (u:href (slots entity) slot) (u:href args key))))
      entity)))

(defun realize-prefab-entity (skeleton path display-id args)
  (let ((types (types skeleton)))
    (with-slots (%mixin-class) skeleton
      (when (typep skeleton 'prefab-entity-skeleton)
        (apply #'change-class skeleton %mixin-class args))
      (setf (node/prefab-path skeleton) path
            (id/display skeleton) display-id)
      (register-entity skeleton types))))

(defun resolve-prefab-entity-args (node parent)
  (with-slots (%prefab %parent %component-args) node
    (let ((args (u:dict #'eq)))
      (u:do-hash (k v (u:href %component-args :resolved))
        (let ((arg (if (functionp v) (funcall v) v)))
          (setf (u:href args k) (if (typep arg 'prefab-reference)
                                    (funcall (func arg) (factory %prefab))
                                    arg))))
      (setf (u:href args :node/parent)
            (if %parent
                (u:href (entities (factory %prefab)) (path %parent))
                (or parent (node-tree (current-scene =context=)))))
      (u:hash->plist args))))

(defun register-prefab-root (prefab)
  (with-slots (%name %root %factory) prefab
    (let ((root (u:href (entities %factory) (path %root))))
      (push root (u:href (prefabs (current-scene =context=)) %name))
      (setf (node/prefab root) %name)
      root)))

(defun build-prefab-factory (prefab)
  (with-slots (%nodes %factory) prefab
    (with-slots (%current-node %entities %func) %factory
      (setf %func (lambda (&key parent)
                    (u:do-hash (path node %nodes)
                      (let ((skeleton (make-prefab-entity-skeleton node)))
                        (setf (u:href %entities path) skeleton)))
                    (u:do-hash (path node %nodes)
                      (setf %current-node node)
                      (let ((args (resolve-prefab-entity-args node parent))
                            (skeleton (u:href %entities path))
                            (display-id (format nil "~{~(~a~)~^ -> ~}" path)))
                        (realize-prefab-entity skeleton path display-id args)))
                    (setf %current-node nil)
                    (register-prefab-root prefab))))))
