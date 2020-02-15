(in-package #:%pyx.prefab)

(defun register-prefab-viewports (entity &key viewports)
  (let ((viewports-table (vp:table (scene:viewports (ctx:current-scene)))))
    (c/node:do-nodes (node :parent entity)
      (when (ent:has-component-p node 'c/render:render)
        (if viewports
            (dolist (name viewports)
              (let ((viewport (u:href viewports-table name)))
                (render:register-order viewport node)))
            (dolist (viewport (vp:get-entity-viewports node))
              (render:register-order viewport node)))))))

(defun load-prefab (name &key viewports parent)
  (let* ((factory (factory (u:href meta:=prefabs= name)))
         (entity (funcall (func factory) :parent parent)))
    (register-prefab-viewports entity :viewports viewports)))

(defun deregister-prefab-entity (entity)
  (a:when-let* ((prefab (c/node:prefab entity))
                (table (scene:prefabs (ctx:current-scene))))
    (a:deletef (u:href table prefab) entity)
    (unless (u:href table prefab)
      (remhash prefab table))))

(defun update-prefab-subtree (prefab)
  (parse-prefab prefab)
  (util::enqueue :recompile (list :prefab (name prefab)))
  (dolist (spec (slaves prefab))
    (let ((slave (u:href meta:=prefabs= spec)))
      (clrhash (nodes slave))
      (update-prefab-subtree slave))))

(util::on-recompile :prefab data ()
  (dolist (entity (u:href (scene:prefabs (ctx:current-scene)) data))
    (let ((parent (c/node:parent entity)))
      (c/node:delete entity)
      (load-prefab data :parent parent))))

;;; Public API

(defmacro define-prefab (name options &body body)
  (a:with-gensyms (data)
    (u:mvlet ((body decls doc (a:parse-body body :documentation t)))
      `(let ((,data (preprocess-prefab-data ,name ,options ,body)))
         (if (u:href meta:=prefabs= ',name)
             (reset-prefab ',name ,data)
             (make-prefab ',name ,data))
         (update-prefab-subtree (u:href meta:=prefabs= ',name))))))
