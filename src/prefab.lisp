(in-package #:pyx)

(defun load-prefab (name &key parent)
  (register-entity-flow-event
   :prefab-create
   (lambda ()
     (let ((factory (factory (meta :prefabs name))))
       (funcall (func factory) :parent parent)))))

(defun recompile-prefab (name)
  (dolist (entity (u:href (prefabs (database *state*)) name))
    (let ((parent (node/parent entity)))
      (delete-entity entity)
      (load-prefab name :parent parent))))

(defun deregister-prefab-entity (entity)
  (a:when-let* ((prefab (node/prefab entity))
                (table (prefabs (database *state*))))
    (a:deletef (u:href table prefab) entity)
    (unless (u:href table prefab)
      (remhash prefab table))))

(defun update-prefab-subtree (prefab)
  (parse-prefab prefab)
  (enqueue :recompile (list :prefab (name prefab)))
  (dolist (spec (slaves prefab))
    (destructuring-bind (type . name) spec
      (ecase type
        (:prefab
         (let ((slave (meta :prefabs name)))
           (clrhash (nodes slave))
           (update-prefab-subtree slave)))
        (:prototype
         (error "Programming error: A prototype cannot be a slave of a prefab. ~
                 This should never happen. Please report this as a bug."))))))

(defmacro define-prefab (name options &body body)
  (a:with-gensyms (prefab data)
    `(let ((,data (preprocess-prefab-data ,name ,options ,body)))
       (unless (meta :prefabs)
         (setf (meta :prefabs) (u:dict #'eq)))
       (if (meta :prefabs ',name)
           (reset-prefab ',name ,data)
           (make-prefab ',name ,data))
       (let ((,prefab (meta :prefabs ',name)))
         (update-prefab-subtree ,prefab)))))
