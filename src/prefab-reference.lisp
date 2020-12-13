(in-package #:ndjinn)

(defun find-prefab-reference-path (factory path)
  (with-slots (%current-node %entities) factory
    (let ((current-path (path %current-node)))
      (u:if-let ((index (search path current-path)))
        (u:href %entities (subseq current-path 0 (+ index (length path))))
        (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}."
               path current-path)))))

(defun find-prefab-reference-down (factory path &optional current)
  (let ((current-path (or current (path (current-node factory)))))
    (or (u:href (entities factory) (append current-path path))
        (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}."
               path current-path))))

(defun find-prefab-reference-up (factory level path)
  (let* ((current-path (path (current-node factory)))
         (parent-path (butlast current-path level)))
    (if path
        (find-prefab-reference-down factory path parent-path)
        (or (u:href (entities factory) parent-path)
            (error "Failed to find parent reference ~{~a~^/~}." parent-path)))))

(defun generate-prefab-reference-func (reference-spec)
  (lambda (factory)
    (destructuring-bind (type . args) reference-spec
      (case type
        (:up
         (destructuring-bind (level &rest path) args
           (find-prefab-reference-up factory level path)))
        (:down
         (find-prefab-reference-down factory args))
        (:path
         (find-prefab-reference-path factory args))
        (:query
         (query (find-prefab-reference-path factory (rest args)) (first args)))
        (t (error "Reference spec type must be :UP, :DOWN, :PATH, or ~
                   :QUERY."))))))

(defun make-prefab-reference (path/query)
  (make-instance 'prefab-reference
                 :func (generate-prefab-reference-func path/query)))
