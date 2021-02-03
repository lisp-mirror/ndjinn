(in-package #:ndjinn)

;;; spec

(defstruct (asset-pool-spec
            (:constructor %make-asset-pool-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (system nil :type symbol)
  (path #p"" :type pathname)
  (assets (u:dict #'eq) :type hash-table))

(defstruct (asset-spec
            (:constructor %make-asset-spec)
            (:predicate nil)
            (:copier nil))
  (pool nil :type symbol)
  (name nil :type symbol)
  (path #p"" :type pathname))

(u:define-printer (asset-pool-spec stream)
  (format stream "~s" (asset-pool-spec-name asset-pool-spec)))

(u:define-printer (asset-spec stream :type nil)
  (format stream "~s (pool: ~s)"
          (asset-spec-name asset-spec)
          (asset-spec-pool asset-spec)))

(defun find-asset-pool (name)
  (u:href =meta/asset-pools= name))

(defun make-asset-spec (pool-name base-path data)
  (destructuring-bind (name path) data
    (let* ((pool (find-asset-pool pool-name))
           (base-path (uiop:ensure-directory-pathname base-path))
           (path (uiop:merge-pathnames* path base-path))
           (asset (%make-asset-spec :pool pool-name :name name :path path)))
      (setf (u:href (asset-pool-spec-assets pool) name) asset)
      asset)))

(defun find-asset-spec (pool-name spec-name)
  (u:if-let ((pool (find-asset-pool pool-name)))
    (or (u:href (asset-pool-spec-assets pool) spec-name)
        (error "Asset ~s not found in pool ~s." spec-name pool-name))
    (error "Asset pool ~s does not exist." pool-name)))

(defun make-asset-symbol (path)
  (intern
   (string-upcase
    (cl-slug:slugify
     (pathname-name path)))))

(defun asset-path-collect-p (path filter)
  (flet ((normalize-type (type)
           (string-downcase (string-left-trim "." type))))
    (let ((path-type (string-downcase (pathname-type path))))
      (some (lambda (x) (string= path-type (normalize-type x)))
            (u:ensure-list filter)))))

(defun update-asset-pool-spec (pool-name system path filter)
  (let* ((pool (find-asset-pool pool-name))
         (path (uiop:ensure-directory-pathname path))
         (resolved-path (%resolve-path system path)))
    (setf (asset-pool-spec-system pool) system
          (asset-pool-spec-path pool) path)
    (clrhash (asset-pool-spec-assets pool))
    (u:map-files
     resolved-path
     (lambda (x)
       (let* ((asset-name (make-asset-symbol x))
              (file-name (file-namestring x))
              (spec (list asset-name file-name)))
         (u:if-found (existing (u:href (asset-pool-spec-assets pool)
                                       asset-name))
           (error "Asset pool ~s has ambiguously named assets:~%~
                     File 1: ~a~%File 2: ~a~%Normalized name: ~a"
                  pool-name
                  file-name
                  (file-namestring (asset-spec-path existing))
                  asset-name)
           (make-asset-spec pool-name path spec))))
     :test (lambda (x) (if filter (asset-path-collect-p x filter) t))
     :recursive-p nil)))

(defun make-asset-pool-spec (name system path filter)
  (let ((pool (%make-asset-pool-spec :name name)))
    (setf (u:href =meta/asset-pools= name) pool)
    (update-asset-pool-spec name system path filter)
    pool))

(defmacro define-asset-pool (name (&key system) &body body)
  (destructuring-bind (&key path filter) body
    `(progn
       (unless ,system
         (error "Asset pool must specify the name of an ASDF system."))
       (if (u:href =meta/asset-pools= ',name)
           (update-asset-pool-spec ',name ,system ,path ',filter)
           (make-asset-pool-spec ',name ,system ,path ',filter)))))

;;; implementation

(defun find-asset (type key)
  (u:href (assets =context=) type key))

(defun delete-asset (type key)
  (remhash key (u:href (assets =context=) type)))

(defun %resolve-path (system path)
  #+(and ndjinn.release sbcl)
  (uiop:merge-pathnames*
   path
   (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))
  #+(and ndjinn.release (not sbcl))
  (error "Release must be deployed on SBCL to load assets.")
  #-ndjinn.release
  (asdf:system-relative-pathname system path))

(defun resolve-system-path (path &optional (system :ndjinn))
  (let* ((system (asdf:find-system system))
         (path (uiop:merge-pathnames*
                path
                (uiop:ensure-directory-pathname "data")))
         (resolved-path (%resolve-path system path)))
    resolved-path))

(defgeneric resolve-path (pool/asset &key))

(defmethod resolve-path ((pool-name symbol) &key)
  (let* ((pool (find-asset-pool pool-name))
         (system (asset-pool-spec-system pool))
         (path (%resolve-path system (asset-pool-spec-path pool))))
    (ensure-directories-exist path)
    path))

(defmethod resolve-path ((pool asset-pool-spec) &key)
  (resolve-path (asset-pool-spec-name pool)))

(defmethod resolve-path ((asset list) &key (not-found-error-p t))
  (destructuring-bind (pool-name spec-name) asset
    (let* ((pool (find-asset-pool pool-name))
           (spec (find-asset-spec pool-name spec-name))
           (system (asset-pool-spec-system pool))
           (path (%resolve-path system (asset-spec-path spec))))
      (if not-found-error-p
          (if (uiop:file-exists-p path)
              (values path spec)
              (error "File path not found for asset ~s of pool ~s.~%Path: ~s."
                     spec-name pool-name path))
          (values path spec)))))

(defmethod resolve-path ((asset string) &key)
  (resolve-system-path asset :ndjinn))

(defmacro with-asset-cache (type key &body body)
  (u:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (assets =context=) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (u:ensure-gethash ,key ,table (progn ,@body)))))
