(in-package #:pyx.web)

(defvar *deployed-p*)

(defun deploy (file)
  #+sbcl
  (progn
    (setf *deployed-p* t)
    (sb-ext:save-lisp-and-die
     file
     :toplevel (lambda ()
                 (start :thread-p nil))
     :executable t
     :compression 9))
  #-sbcl
  (error "Deployment is not supported on ~a."
         (lisp-implementation-type)))
