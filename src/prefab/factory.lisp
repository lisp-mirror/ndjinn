(in-package #:net.mfiano.lisp.pyx)

(defun make-prefab-entity (node)
  (%make-entity (u:href (component-types node) :resolved)))

(defun realize-prefab-entity (entity path display-id args)
  (apply #'reinitialize-instance entity args)
  (setf (node/prefab-path entity) path
        (id/display entity) display-id)
  (register-entity entity)
  entity)

(defun resolve-prefab-entity-args (node root)
  (let ((factory (factory (prefab node)))
        (args (u:dict #'eq)))
    (u:do-hash (k v (u:href (component-args node) :resolved))
      (let ((arg (if (functionp v) (funcall v) v)))
        (setf (u:href args k) (if (typep arg 'prefab-reference)
                                  (funcall (func arg) factory)
                                  arg))))
    (setf (u:href args :node/parent)
          (u:if-let ((parent (parent node)))
            (u:href (entities factory) (path parent))
            (or root (node-tree (current-scene =context=)))))
    (u:hash->plist args)))

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
                      (let ((entity (make-prefab-entity node)))
                        (setf (u:href %entities path) entity)))
                    (u:do-hash (path node %nodes)
                      (setf %current-node node)
                      (let ((args (resolve-prefab-entity-args node parent))
                            (entity (u:href %entities path))
                            (display-id (format nil "~{~(~a~)~^ -> ~}" path)))
                        (realize-prefab-entity entity path display-id args)))
                    (setf %current-node nil)
                    (register-prefab-root prefab))))))
