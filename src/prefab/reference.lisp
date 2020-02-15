(in-package #:%pyx.prefab)

(defun find-prefab-reference (factory path)
  (with-slots (%current-node %entities) factory
    (let ((current-path (path %current-node)))
      (a:if-let ((index (search path current-path)))
        (u:href %entities (subseq current-path 0 (+ index (length path))))
        (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}."
               path current-path)))))

(defun generate-prefab-reference-func (path/query)
  (lambda (factory)
    (let ((query (first (last path/query))))
      (if (keywordp query)
          (ent:query (find-prefab-reference factory (butlast path/query)) query)
          (find-prefab-reference factory path/query)))))

(defun make-prefab-reference (path/query)
  (make-instance 'prefab-reference
                 :func (generate-prefab-reference-func path/query)))
