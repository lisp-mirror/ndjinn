(in-package #:%pyx.scene)

(defstruct (spec (:constructor %make-spec)
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  name
  pass-order
  pass-table
  draw-order
  collider-plan
  sub-trees
  spec-viewports)

(u:define-printer (spec stream)
  (format stream "~s" (name spec)))

(defun make-scene-draw-order-table (order)
  (loop :with table = (u:dict #'eq :default 0)
        :for item :in order
        :for i :from 0
        :do (setf (u:href table item) i)
        :finally (return table)))

(defun update-spec (name sub-trees viewports passes draw-order collider-plan)
  (let* ((spec (u:href meta:=scenes= name))
         (draw-order-table (make-scene-draw-order-table draw-order))
         (pass-table (u:dict #'eq))
         (sub-trees (mapcar
                     (lambda (x)
                       (if (listp x) x (list x x)))
                     sub-trees))
         (viewports (or viewports
                        `((:default ,(mapcar #'car sub-trees))))))
    (dolist (pass passes)
      (a:if-let ((pass-spec (u:href meta:=render-passes= pass)))
        (setf (u:href pass-table pass) pass-spec)
        (error "Render pass ~s not defined." pass)))
    (setf (sub-trees spec) sub-trees
          (spec-viewports spec) viewports
          (pass-order spec) passes
          (pass-table spec) pass-table
          (draw-order spec) draw-order-table
          (collider-plan spec) collider-plan)
    (tp:enqueue :recompile (list :scene name))))

(defun make-spec (name sub-trees viewports passes draw-order collider-plan)
  (let ((spec (%make-spec :name name)))
    (setf (u:href meta:=scenes= name) spec)
    (update-spec name sub-trees viewports passes draw-order collider-plan)
    spec))

(live:on-recompile :scene data ()
  (let ((scene (ctx:current-scene)))
    (with-slots (%spec %prefabs %loaded-p) scene
      (when (eq data (name %spec))
        (u:do-hash-values (entities %prefabs)
          (map nil #'c/node:delete entities))
        (c/node:delete (node-tree scene))
        (setf %loaded-p nil)
        (load-scene data)))))

;;; Public API

(defun get-registered-scene-names (&optional package-name)
  (let ((package-name (or package-name *package*)))
    (remove-if-not
     (lambda (x)
       (eq (symbol-package x) (find-package package-name)))
     (sort (u:hash-keys meta:=scenes=) #'string<))))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key sub-trees viewports (passes '(:default))
                         (draw-order '(:default))
                         (collider-plan :default))
      (car body)
    `(if (u:href meta:=scenes= ',name)
         (update-spec ',name ',sub-trees ',viewports ',passes ',draw-order
                      ',collider-plan)
         (make-spec ',name ',sub-trees ',viewports ',passes ',draw-order
                    ',collider-plan))))
