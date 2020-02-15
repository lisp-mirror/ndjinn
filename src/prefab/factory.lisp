(in-package #:%pyx.prefab)

(defun make-prefab-entity-skeleton (node)
  (with-slots (%component-types %component-args) node
    (let* ((types (u:href %component-types :resolved))
           (args (u:href %component-args :resolved))
           (entity (make-instance 'prefab-entity-skeleton
                                  :mixin-class (ent:make-class types)
                                  :types types
                                  :args args)))
      (dolist (slot (ent:get-slots types))
        (let ((key (a:make-keyword (string-left-trim "%" (symbol-name slot)))))
          (setf (u:href (slots entity) slot) (u:href args key))))
      entity)))

(defun realize-prefab-entity (skeleton args)
  (let ((types (types skeleton)))
    (with-slots (%mixin-class) skeleton
      (when (typep skeleton 'prefab-entity-skeleton)
        (apply #'change-class skeleton %mixin-class args))
      (ent:register skeleton types))))

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
                (or parent (scene:node-tree (ctx:current-scene)))))
      (u:hash->plist args))))

(defun register-prefab-root (prefab)
  (with-slots (%name %root %factory) prefab
    (let ((root (u:href (entities %factory) (path %root))))
      (push root (u:href (scene:prefabs (ctx:current-scene)) %name))
      (setf (c/node:prefab root) %name)
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
                        (realize-prefab-entity skeleton args)
                        (setf (c/node:prefab-path skeleton) path
                              (c/id:display skeleton) display-id)))
                    (setf %current-node nil)
                    (register-prefab-root prefab))))))
