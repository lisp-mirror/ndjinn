(in-package #:pyx)

(defvar *state* nil)

(defmacro make-nested-dict (test-func &rest keys)
  `(u:dict ,test-func
           ,@(mapcan
              (lambda (x)
                `(,x (u:dict ,test-func)))
              keys)))
