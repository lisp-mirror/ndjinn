(in-package #:%pyx.resource)

;;; Public API

(defun resolve-path (path)
  (uiop:merge-pathnames*
   (uiop:ensure-directory-pathname "data")
   (if cfg:=release=
       #+sbcl
       (truename
        (uiop:merge-pathnames*
         path
         (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*))))
       #-sbcl
       (error "Release must be deployed on SBCL to be able to load resourcess.")
       (asdf:system-relative-pathname (asdf:find-system :pyx) path))))

(defmacro with-resource-cache (type key &body body)
  (a:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (ctx:resources) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (a:ensure-gethash ,key ,table (progn ,@body)))))

(defun find-resource (type key)
  (u:href (ctx:resources) type key))

(defun delete-resource (type key)
  (remhash key (u:href (ctx:resources) type)))