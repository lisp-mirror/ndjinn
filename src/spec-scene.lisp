(in-package #:pyx)

(defclass scene-spec ()
  ((%name :reader name
          :initarg :name)
   (%pass-order :reader pass-order)
   (%pass-table :reader pass-table)
   (%draw-order :reader draw-order)
   (%prefabs :reader prefabs)))

(u:define-printer (scene-spec stream)
  (format stream "~s" (name scene-spec)))

(define-event-handler :recompile :scene recompile-scene)

(defun make-scene-draw-order-table (order)
  (loop :with table = (u:dict #'eq :default 0)
        :for item :in order
        :for i :from 0
        :do (setf (u:href table item) i)
        :finally (return table)))

(defun update-scene-spec (name prefabs passes draw-order)
  (with-slots (%prefabs %pass-order %pass-table %draw-order)
      (meta :scenes name)
    (let ((draw-order-table (make-scene-draw-order-table draw-order))
          (pass-table (u:dict #'eq)))
      (dolist (pass passes)
        (a:if-let ((pass-spec (meta :render-passes pass)))
          (setf (u:href pass-table pass) pass-spec)
          (error "Render pass ~s not defined." pass)))
      (setf %prefabs prefabs
            %pass-order passes
            %pass-table pass-table
            %draw-order draw-order-table)
      (enqueue :recompile (list :scene name)))))

(defun make-scene-spec (name prefabs passes draw-order)
  (let ((spec (make-instance 'scene-spec :name name)))
    (setf (meta :scenes name) spec)
    (update-scene-spec name prefabs passes draw-order)
    spec))

(defmacro define-scene (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key prefabs (passes '(:default))
                         (draw-order '(:default)))
      (car body)
    `(progn
       (unless (meta :scenes)
         (setf (meta :scenes) (u:dict #'eq)))
       (if (meta :scenes ',name)
           (update-scene-spec ',name ',prefabs ',passes ',draw-order)
           (make-scene-spec ',name ',prefabs ',passes ',draw-order)))))
