(in-package #:%pyx.asset)

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
