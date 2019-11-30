(in-package #:pyx)

(defvar *state* nil)

(defmacro assert! (&rest body)
  (if (find :pyx.release *features*)
      `(progn)
      `(progn
         ,@(mapcar
            (lambda (x)
              `(assert ,x))
            body))))

(defmacro make-nested-dict (test-func &rest keys)
  `(u:dict ,test-func
           ,@(mapcan
              (lambda (x)
                `(,x (u:dict ,test-func)))
              keys)))

(defun rng/init ()
  (setf *random-state* (make-random-state t)))
