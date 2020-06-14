(in-package #:net.mfiano.lisp.pyx)

(defmacro make-nested-dict (test-func &rest keys)
  `(u:dict ,test-func ,@(mapcan (lambda (x) `(,x (u:dict ,test-func))) keys)))

(defun initialize-rng ()
  (setf *random-state* (make-random-state t)))
