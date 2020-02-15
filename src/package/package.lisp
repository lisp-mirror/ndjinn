(in-package #:cl-user)

(defpackage #:%pyx.package
  (:use #:cl)
  (:import-from
   #+sbcl #:sb-ext
   #+ccl #:ccl
   #+(or ecl abcl clasp) #:ext
   #+lispworks #:hcl
   #+allegro #:excl
   #:add-package-local-nickname)
  (:export
   #:define-nicknames
   #:define-package))

(in-package #:%pyx.package)

(defmacro define-package (package &body body)
  `(defpackage ,package
     ,@(mapcan
        (lambda (x)
          (destructuring-bind (option . items) x
            (case option
              (:inherit-from
               (destructuring-bind (from . symbols) items
                 `((:import-from ,from ,@symbols)
                   (:export ,@symbols))))
              (t `(,x)))))
        body)))

(defmacro define-local-nicknames (package &body body)
  `(progn
     ,@(mapcar
        (lambda (x)
          (destructuring-bind (nickname target) x
            `(add-package-local-nickname ',nickname ',target ',package)))
        body)))
