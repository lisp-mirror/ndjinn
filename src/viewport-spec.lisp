(in-package #:%pyx.viewport)

(defstruct (spec (:constructor %make-spec)
                 (:conc-name spec-)
                 (:predicate nil)
                 (:copier nil))
  name
  (x 0)
  (y 0)
  (width 1)
  (height 1))

(u:define-printer (spec stream :identity t)
  (format stream "~s" (spec-name spec)))

(defun update-spec (name x y width height)
  (let ((spec (u:href meta:=viewports= name)))
    (setf (spec-x spec) (a:clamp (float x 1f0) 0f0 1f0)
          (spec-y spec) (a:clamp (float y 1f0) 0f0 1f0)
          (spec-width spec) (a:clamp (float width 1f0) 0f0 1f0)
          (spec-height spec) (a:clamp (float height 1f0) 0f0 1f0))
    (tp:enqueue :recompile (list :viewport))))

(defun make-spec (name x y width height)
  (let ((spec (%make-spec :name name)))
    (setf (u:href meta:=viewports= name) spec)
    (update-spec name x y width height)
    spec))

;;; Public API

(defmacro define-viewport (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key (x 0) (y 0) (width 1) (height 1)) (car body)
    `(if (u:href meta:=viewports= ',name)
         (update-spec ',name ,x ,y ,width ,height)
         (make-spec ',name ,x ,y ,width ,height))))
