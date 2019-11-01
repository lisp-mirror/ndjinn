(in-package #:pyx)

(defun transform-prefab-reference (factory node)
  (with-slots (%prefab-name %current-node) factory
    (let ((path (path %current-node)))
      (when node
        (find-if
         (lambda (x) (subseq path 0 (mismatch path x)))
         (u:href (references node) %prefab-name))))))

(defun find-prefab-reference/node (factory path)
  (with-slots (%current-node %entities) factory
    (let* ((nodes (nodes (prefab (template %current-node))))
           (new-path (transform-prefab-reference factory (u:href nodes path))))
      (or (u:href %entities new-path)
          (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}."
                 path (path %current-node))))))

(defun find-prefab-reference (factory path)
  (with-slots (%current-node %entities) factory
    (etypecase (template %current-node)
      (prototype (u:href %entities path))
      (prefab-node (find-prefab-reference/node factory path)))))

(defun generate-prefab-reference-func (path/query)
  (lambda (factory)
    (let ((query (first (last path/query))))
      (if (keywordp query)
          (%entity-query (find-prefab-reference factory (butlast path/query))
                         query)
          (find-prefab-reference factory path/query)))))

(defun make-prefab-reference (path/query)
  (make-instance 'prefab-reference
                 :func (generate-prefab-reference-func path/query)))
