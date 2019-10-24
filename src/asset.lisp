(in-package #:pyx)

(defun resolve-asset-path (path)
  (uiop:merge-pathnames*
   (uiop:ensure-directory-pathname "data")
   (if (cfg :release)
       #+sbcl
       (truename
        (uiop:merge-pathnames*
         path
         (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*))))
       #-sbcl
       (error "Releases must be deployed on SBCL to be able to load assets.")
       (asdf:system-relative-pathname (asdf:find-system :pyx) path))))

(defmacro resource-lookup (type key &body body)
  (a:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (resources *state*) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (a:ensure-gethash ,key ,table (progn ,@body)))))
