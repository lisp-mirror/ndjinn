(in-package #:net.mfiano.lisp.pyx)

(defstruct (geometry-spec
            (:constructor %make-geometry-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (id 0 :type fixnum)
  (layout (%make-geometry-layout) :type geometry-layout)
  (buffers (vector) :type vector)
  (buffer-names (u:dict #'eq) :type hash-table)
  (primitive :triangles :type keyword)
  (vertex-count 0 :type u:ub32)
  (primitive-count 0 :type u:ub32))

(defun make-geometry-spec (layout-name
                           &key name (primitive :triangles) (vertex-count 0)
                             buffer-data)
  (lambda ()
    (let ((spec (%make-geometry-spec
                 :name name
                 :id (gl:gen-vertex-array)
                 :layout (find-geometry-layout layout-name)
                 :vertex-count vertex-count
                 :primitive primitive)))
      (gl:bind-vertex-array (geometry-spec-id spec))
      (make-geometry-buffers spec)
      (configure-geometry-buffers spec)
      (u:do-plist (k v buffer-data)
        (update-geometry spec k v))
      spec)))

(defmacro define-geometry (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key layout (primitive :triangles) (vertex-count 0)
                         buffers)
      (car body)
    `(setf (u:href =geometry= ',name)
           (make-geometry-spec ',layout
                               :name ',name
                               :primitive ',primitive
                               :vertex-count ,vertex-count
                               :buffer-data ',buffers))))
