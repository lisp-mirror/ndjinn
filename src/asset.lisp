(in-package #:%pyx.asset)

(defstruct (cached-asset (:conc-name nil)
                         (:predicate nil)
                         (:copier nil))
  (references 1)
  value)

(defun find-asset (type key)
  (a:when-let* ((assets (ctx:assets))
                (type (u:href assets type))
                (asset (u:href type key)))
    (value asset)))

(defgeneric delete-asset (type key)
  (:method (type key))
  (:method :around (type key)
    (let ((asset (u:href (ctx:assets) type key)))
      (decf (references asset))
      (when (zerop (references asset))
        (call-next-method)
        (remhash key (u:href (ctx:assets) type))
        (when (zerop (hash-table-count (u:href (ctx:assets) type)))
          (remhash type (ctx:assets)))))))

(defun %resolve-path (system path)
  (if cfg:=release=
      #+sbcl
      (uiop:merge-pathnames*
       path
       (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))
      #-sbcl
      (error "Release must be deployed on SBCL to load assets.")
      (asdf:system-relative-pathname system path)))

(defgeneric resolve-path (data))

(defmethod resolve-path ((data list))
  (destructuring-bind (pool-name spec-name) data
    (let* ((spec (find-spec pool-name spec-name))
           (system (get-pool-system pool-name))
           (path (%resolve-path system (path spec))))
      (if (uiop:file-exists-p path)
          (values path spec)
          (error "File path not found for asset ~s of pool ~s.~%Path: ~s."
                 spec-name pool-name path)))))

(defmethod resolve-path ((data string))
  (let* ((system (asdf:find-system :pyx))
         (path (uiop:merge-pathnames*
                data
                (uiop:ensure-directory-pathname "data")))
         (resolved-path (%resolve-path system path)))
    (if (uiop:file-exists-p resolved-path)
        resolved-path
        (error "File path not found: ~s." resolved-path))))

(defmacro with-asset-cache (type key &body body)
  (a:with-gensyms (asset)
    `(progn
       (unless (u:href (ctx:assets) ,type)
         (setf (u:href (ctx:assets) ,type) (u:dict #'equal)))
       (a:if-let ((,asset (u:href (ctx:assets) ,type ,key)))
         (progn
           (incf (references ,asset))
           (value ,asset))
         (let ((,asset (make-cached-asset :value (progn ,@body))))
           (setf (u:href (ctx:assets) ,type ,key) ,asset)
           (value ,asset))))))
