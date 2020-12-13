(in-package #:ndjinn)

(defstruct (geometry-spec
            (:constructor %make-geometry-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (id 0 :type u:ub32)
  (layout (%make-geometry-layout-spec) :type geometry-layout-spec)
  (buffers (vector) :type vector)
  (buffer-names (u:dict #'eq) :type hash-table)
  (primitive :triangles :type keyword)
  (vertex-count 0 :type u:ub32)
  (primitive-count 0 :type u:ub32))

(defmacro define-geometry (name options &body body)
  (declare (ignore options))
  (u:with-gensyms (spec func)
    (destructuring-bind (&key layout (primitive :triangles) (vertex-count 0)
                           buffers)
        (car body)
      `(let ((,func (lambda ()
                      (let ((,spec (%make-geometry-spec
                                    :name ',name
                                    :id (gl:gen-vertex-array)
                                    :layout (find-geometry-layout ',layout)
                                    :vertex-count ,vertex-count
                                    :primitive ',primitive)))
                        (gl:bind-vertex-array (geometry-spec-id ,spec))
                        (make-geometry-buffers ,spec)
                        (configure-geometry-buffers ,spec)
                        ,@(loop :for (k v) :on buffers :by #'cddr
                                :collect `(%update-geometry ,spec ,k ',v))
                        ,spec))))
         (setf (u:href =meta/geometry= ',name) ,func)))))
