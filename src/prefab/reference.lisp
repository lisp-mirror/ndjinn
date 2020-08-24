(in-package #:net.mfiano.lisp.pyx)

(defun find-prefab-reference-path (factory path)
  (with-slots (%current-node %entities) factory
    (let ((current-path (path %current-node)))
      (u:if-let ((index (search path current-path)))
        (u:href %entities (subseq current-path 0 (+ index (length path))))
        (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}."
               path current-path)))))

(defun find-prefab-reference-parent (factory level)
  (with-slots (%current-node %entities) factory
    (let* ((current-path (path %current-node))
           (parent-path (butlast current-path level)))
      (or (u:href %entities parent-path)
          (error "Failed to find parent reference ~{~a~^/~}" parent-path)))))

(defun generate-prefab-reference-func (reference-spec)
  (lambda (factory)
    (destructuring-bind (type . args) reference-spec
      (case type
        (:parent
         (when (> (length args) 1)
           (error "Parent reference takes 1 argument; the number of nodes ~
                   upward from this node."))
         (find-prefab-reference-parent factory (or (first args) 1)))
        (:path
         (find-prefab-reference-path factory args))
        (:query
         (query (find-prefab-reference-path factory (rest args)) (first args)))
        (t (error "Reference spec type must be :PARENT, :PATH, or :QUERY."))))))

(defun make-prefab-reference (path/query)
  (make-instance 'prefab-reference
                 :func (generate-prefab-reference-func path/query)))
