(in-package #:%pyx.geometry)

(defstruct (spec (:constructor %make-spec)
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  (id (gl:gen-vertex-array))
  layout
  buffers
  (buffer-names (u:dict #'eq))
  primitive
  (primitive-count 0))

(defun make-spec (layout-name
                  &key (primitive :triangles) buffer-data)
  (lambda ()
    (let ((spec (%make-spec :layout (find-layout layout-name)
                            :primitive primitive)))
      (gl:bind-vertex-array (id spec))
      (make-buffers spec)
      (configure-buffers spec)
      (u:do-plist (k v buffer-data)
        (update-geometry spec k v))
      spec)))

;;; Public API

(defmacro define-geometry (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key layout (primitive :triangles) buffers)
      (car body)
    `(setf (u:href meta:=geometry= ',name)
           (make-spec ',layout
                      :primitive ',primitive
                      :buffer-data ',buffers))))
