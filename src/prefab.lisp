(in-package #:pyx)

(defun split-prefab-spec (spec)
  (destructuring-bind ((name . components) . (body)) spec
    (loop :for tail :on body
          :for item = (first tail)
          :while (symbolp (first item))
          :finally (return (values name components body)))))

(defmacro define-prefab (prefab-name args2 &body body)
  (declare (ignore prefab-name args2))
  (labels ((rec (data)
             (destructuring-bind ((name . components) . children) data
               `(list ',name
                      ,@(mapcar #'thunk components)
                      ,@(mapcar #'rec children))))
           (thunk (data)
             (destructuring-bind (type . args) data
               `(list ',type ,@args))))
    `(list ,@(mapcar #'rec body))))
