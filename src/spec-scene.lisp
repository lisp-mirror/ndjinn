(in-package #:pyx)

(defclass scene-spec ()
  ((%name :reader name
          :initarg :name)
   (%pass-order :reader pass-order)
   (%pass-table :reader pass-table)
   (%draw-order :reader draw-order)
   (%collider-plan :reader collider-plan)
   (%sub-trees :reader sub-trees)
   (%viewports :reader viewports)))

(u:define-printer (scene-spec stream)
  (format stream "~s" (name scene-spec)))

(define-event-handler :recompile :scene recompile-scene)

(defun make-scene-draw-order-table (order)
  (loop :with table = (u:dict #'eq 'default 0)
        :for item :in order
        :for i :from 0
        :do (setf (u:href table item) i)
        :finally (return table)))

(defun update-scene-spec (name sub-trees viewports passes draw-order
                          collider-plan)
  (with-slots (%sub-trees %viewports %pass-order %pass-table %draw-order
               %collider-plan)
      (meta :scenes name)
    (let* ((draw-order-table (make-scene-draw-order-table draw-order))
           (pass-table (u:dict #'eq))
           (sub-trees (mapcar
                       (lambda (x)
                         (if (listp x) x (list x x)))
                       sub-trees))
           (viewports (or viewports `((default ,(mapcar #'car sub-trees))))))
      (dolist (pass passes)
        (a:if-let ((pass-spec (meta :render-passes pass)))
          (setf (u:href pass-table pass) pass-spec)
          (error "Render pass ~s not defined." pass)))
      (setf %sub-trees sub-trees
            %viewports viewports
            %pass-order passes
            %pass-table pass-table
            %draw-order draw-order-table
            %collider-plan collider-plan)
      (enqueue :recompile (list :scene name)))))

(defun make-scene-spec (name sub-trees viewports passes draw-order
                        collider-plan)
  (let ((spec (make-instance 'scene-spec :name name)))
    (setf (meta :scenes name) spec)
    (update-scene-spec name sub-trees viewports passes draw-order collider-plan)
    spec))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key sub-trees viewports (passes '(default))
                         (draw-order '(default)) (collider-plan 'default))
      (car body)
    `(progn
       (unless (meta :scenes)
         (setf (meta :scenes) (u:dict #'eq)))
       (if (meta :scenes ',name)
           (update-scene-spec
            ',name ',sub-trees ',viewports ',passes ',draw-order
            ',collider-plan)
           (make-scene-spec
            ',name ',sub-trees ',viewports ',passes ',draw-order
            ',collider-plan)))))
