(in-package #:pyx)

(defclass geometry-spec ()
  ((%id :reader id
        :initform (gl:create-vertex-array))
   (%layout :reader layout
            :initarg :layout)
   (%buffers :reader buffers)
   (%buffer-names :reader buffer-names
                  :initform (u:dict #'eq))
   (%primitive :reader primitive
               :initarg :primitive)
   (%primitive-count :reader primitive-count
                     :initform 0)
   (%vertex-count :reader vertex-count
                  :initarg :vertex-count)))

(defun make-geometry-func (layout-name
                           &key (primitive :triangles) (vertex-count 0)
                             buffer-data)
  (lambda ()
    (let ((geometry (make-instance 'geometry-spec
                                   :layout (find-geometry-layout layout-name)
                                   :primitive primitive
                                   :vertex-count vertex-count)))
      (gl:bind-vertex-array (id geometry))
      (make-geometry-buffers geometry)
      (configure-geometry-buffers geometry)
      (u:do-plist (k v buffer-data)
        (update-geometry geometry k v))
      geometry)))

(defun make-geometry (name)
  (funcall (u:href (meta :geometry) name)))

(defun update-geometry (geometry buffer-name data)
  (with-slots (%vertex-count %primitive-count) geometry
    (let ((data (or data (make-array %vertex-count :initial-element 0))))
      (fill-geometry-buffer geometry buffer-name data)
      (setf %primitive-count (length data))
      (u:noop))))

(defun draw-geometry (geometry instance-count)
  (with-slots (%id %primitive %primitive-count %vertex-count) geometry
    (gl:bind-vertex-array %id)
    (%gl:draw-arrays-instanced
     %primitive 0 (* %primitive-count %vertex-count) instance-count)))

(defun delete-geometry (geometry)
  (gl:delete-buffers (list (id geometry))))

(defmacro define-geometry (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key layout (primitive :triangles) (vertex-count 0)
                         buffers)
      (car body)
    `(progn
       (unless (meta :geometry)
         (setf (meta :geometry) (u:dict #'eq)))
       (setf (meta :geometry ',name)
             (make-geometry-func ',layout
                                 :primitive ',primitive
                                 :vertex-count ,vertex-count
                                 :buffer-data ',buffers)))))
