(in-package #:%pyx.asset)

;;; spec

(defstruct (spec (:constructor %make-spec)
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  pool
  name
  path)

(u:define-printer (spec stream :type nil)
  (format stream "~s (pool: ~s)" (name spec) (pool spec)))

(defun make-spec (pool-name base-path data)
  (destructuring-bind (name path) data
    (let* ((success nil)
           (pool (find-pool pool-name))
           (base-path (uiop:ensure-directory-pathname base-path))
           (path (uiop:merge-pathnames* path base-path))
           (asset (%make-spec :pool pool-name :name name :path path)))
      (setf (u:href pool name) asset)
      (unwind-protect (setf success (resolve-path (list pool-name name)))
        (unless success
          (remhash name pool)))
      asset)))

(defun find-spec (pool-name spec-name)
  (a:if-let ((pool (find-pool pool-name)))
    (or (u:href pool spec-name)
        (error "Asset ~s not found in pool ~s." spec-name pool-name))
    (error "Asset pool ~s does not exist." pool-name)))

(defun find-pool (name)
  (u:href meta:=asset-pools= name))

(defun get-pool-system (pool-name)
  (let ((package-name (package-name (symbol-package pool-name))))
    (or (asdf:find-system (a:make-keyword package-name) nil)
        (error "Asset pool ~s must be defined in a package with the same name ~
                as its ASDF system."
               pool-name))))

(defun update-asset-pool (pool-name base-path asset-specs)
  (let ((pool (find-pool pool-name)))
    (clrhash pool)
    (dolist (spec asset-specs)
      (make-spec pool-name base-path spec))))

(defun make-asset-pool (name base-path asset-specs)
  (let ((pool (u:dict #'eq)))
    (setf (u:href meta:=asset-pools= name) pool)
    (update-asset-pool name base-path asset-specs)
    pool))

(defmacro define-asset-pool (name (&key base) &body body)
  `(if (u:href meta:=asset-pools= ',name)
       (update-asset-pool ',name ,base ',body)
       (make-asset-pool ',name ,base ',body)))

;;; implementation

(defun find-asset (type key)
  (u:href (ctx:assets) type key))

(defun delete-asset (type key)
  (remhash key (u:href (ctx:assets) type)))

(defun %resolve-path (system path)
  (if cfg:=release=
      #+sbcl
      (uiop:merge-pathnames*
       path
       (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))
      #-sbcl
      (error "Release must be deployed on SBCL to load assets.")
      (asdf:system-relative-pathname system path)))

(defun resolve-system-path (path &optional (system :pyx))
  (let* ((system (asdf:find-system system))
         (path (uiop:merge-pathnames*
                path
                (uiop:ensure-directory-pathname "data")))
         (resolved-path (%resolve-path system path)))
    resolved-path))

(defgeneric resolve-path (asset))

(defmethod resolve-path ((asset list))
  (destructuring-bind (pool-name spec-name) asset
    (let* ((spec (find-spec pool-name spec-name))
           (system (get-pool-system pool-name))
           (path (%resolve-path system (path spec))))
      (if (uiop:file-exists-p path)
          (values path spec)
          (error "File path not found for asset ~s of pool ~s.~%Path: ~s."
                 spec-name pool-name path)))))

(defmethod resolve-path ((asset string))
  (resolve-system-path asset :pyx))

(defmacro with-asset-cache (type key &body body)
  (a:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (ctx:assets) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (a:ensure-gethash ,key ,table (progn ,@body)))))
