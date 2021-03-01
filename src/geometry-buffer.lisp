(in-package #:ndjinn)

(defun get-geometry-buffer-names (group)
  (ecase (geometry-group-format group)
    (:separate
     (let (names)
       (u:do-hash-keys (k (geometry-group-attributes group))
         (push (u:format-symbol :keyword "~a/~a" (geometry-group-name group) k)
               names))
       (nreverse names)))
    (:interleaved
     (list (geometry-group-name group)))))

(defun make-geometry-buffers (geometry)
  (let ((layout (geometry-spec-layout geometry))
        (buffers (make-array 0 :fill-pointer 0 :adjustable t)))
    (setf (geometry-spec-buffers geometry) buffers)
    (dolist (group-name (geometry-layout-spec-group-order layout))
      (let ((group (u:href (geometry-layout-spec-groups layout) group-name)))
        (dolist (name (get-geometry-buffer-names group))
          (let ((buffer (gl:gen-buffer)))
            (setf (u:href (geometry-spec-buffer-names geometry) name) buffer)
            (vector-push-extend buffer (geometry-spec-buffers geometry))))))))

(defun configure-geometry-buffers (geometry)
  (let ((layout (geometry-spec-layout geometry))
        (buffer-offset 0)
        (attribute-offset 0))
    (dolist (group-name (geometry-layout-spec-group-order layout))
      (let* ((group (u:href (geometry-layout-spec-groups layout) group-name))
             (buffer-count (get-geometry-group-buffer-count group))
             (group-buffers (make-array
                             buffer-count
                             :displaced-to (geometry-spec-buffers geometry)
                             :displaced-index-offset buffer-offset))
             (attribute-count (length (geometry-group-attribute-order group))))
        (dotimes (i attribute-count)
          (gl:enable-vertex-attrib-array (+ attribute-offset i)))
        (configure-geometry-group group attribute-offset group-buffers)
        (incf buffer-offset buffer-count)
        (incf attribute-offset attribute-count)))))

(u:fn-> get-geometry-buffer-size (vector) fixnum)
(defun get-geometry-buffer-size (buffer)
  (declare (optimize speed))
  (* (the (u:ub32) (length buffer))
     (etypecase buffer
       (u:b8a 1)
       (u:ub8a 1)
       (u:b16a 2)
       (u:ub16a 2)
       (u:b32a 4)
       (u:ub32a 4)
       (u:f32a 4)
       (u:f64a 8))))

(defmacro with-geometry-buffer ((ptr size vector) &body body)
  (u:with-gensyms (sv)
    `(sv:with-static-vector
         (,sv (length ,vector)
              :element-type (array-element-type ,vector)
              :initial-contents ,vector)
       (let ((,size (get-geometry-buffer-size ,vector))
             (,ptr (sv:static-vector-pointer ,sv)))
         ,@body))))

(defun fill-geometry-buffer (geometry buffer-name data
                             &key (usage :dynamic-draw))
  (let ((data (u:flatten-numbers data)))
    (with-geometry-buffer (ptr size data)
      (let ((buffer (u:href (geometry-spec-buffer-names geometry) buffer-name)))
        (gl:bind-buffer :array-buffer buffer)
        (%gl:buffer-data :array-buffer size ptr usage)
        (gl:bind-buffer :array-buffer 0)
        (values)))))
