(in-package #:pyx)

(defmacro make-nested-dict (test-func &rest keys)
  `(u:dict ,test-func ,@(mapcan (lambda (x) `(,x (u:dict ,test-func))) keys)))

(defun initialize-rng ()
  (setf *random-state* (make-random-state t)))

(defun split-string (string delimiter)
  (let ((pos (position delimiter string)))
    (values (subseq string 0 pos)
            (subseq string (1+ pos)))))
