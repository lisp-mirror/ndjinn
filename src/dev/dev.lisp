(in-package #:pyx)

(defpackage #:pyx.dev
  (:local-nicknames
   (#:asset #:%pyx.asset)
   (#:lp #:lparallel)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:pyx.dev)

(defun optimize-png-images ()
  (let ((lp:*kernel* (lp:make-kernel 24))
        (images nil))
    (u:map-files (asset:resolve-system-path "textures/" :pyx.extension)
                 (lambda (x)
                   (u:with-binary-input (in x)
                     (push (cons (namestring x) (file-length in)) images)))
                 :test (lambda (x) (string= (pathname-type x) "png")))
    (lp:pmap
     nil
     (lambda (x)
       (let* ((path (car x))
              (command (format nil "pngcrush -brute -reduce ~s ~s.out"
                               path
                               path)))
         (uiop:run-program command)
         (delete-file path)
         (rename-file (format nil "~a.out" path) path)))
     (sort images #'> :key #'cdr))
    (lp:end-kernel :wait t)
    (values)))
