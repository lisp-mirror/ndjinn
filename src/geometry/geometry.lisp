(in-package #:net.mfiano.lisp.pyx)

(defun make-geometry (name)
  (let ((geometry (funcall (u:href =geometry= name))))
    (log:debug :pyx.core "Created geometry: ~s (VAO: ~d)" name (id geometry))
    geometry))

(defun update-geometry (geometry buffer-name data)
  (let ((data (or data (make-array (vertex-count geometry)
                                   :initial-element 0))))
    (fill-geometry-buffer geometry buffer-name data)
    (setf (primitive-count geometry) (length data))))

(defun draw-geometry (geometry instance-count)
  (gl:bind-vertex-array (id geometry))
  (%gl:draw-arrays-instanced (primitive geometry)
                             0
                             (* (primitive-count geometry)
                                (vertex-count geometry))
                             instance-count)
  (gl:bind-vertex-array 0))

(defun delete-geometry (geometry)
  (let ((id (id geometry)))
    (gl:delete-buffers (buffers geometry))
    (gl:delete-vertex-arrays (list id))
    (log:debug :pyx.core "Deleted geometry: ~s (VAO: ~d)" (name geometry) id)))
