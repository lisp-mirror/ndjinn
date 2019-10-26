(in-package #:pyx.web)

(defun %get-table-options (spec)
  (loop :for (column-name . data) :in spec
        :for options = (mapcar
                        (lambda (x) `',x)
                        (u:plist-remove data :func :ref))
        :collect `(,column-name ,@options)))

(defun %generate-foreign-keys (spec)
  (loop :for (column-name . options) :in spec
        :for ref = (u:plist-get options :ref)
        :when ref
          :collect `(sql:foreign-key
                     ,(a:make-keyword column-name)
                     :references
                     ',(subseq (mapcar #'a:make-keyword ref) 0 2)
                     ,@(subseq ref 2))))

(defun %generate-inflators (spec)
  (loop :for (column-name . options) :in spec
        :for ref = (u:plist-get options :ref)
        :for func = (u:plist-get options :func)
        :for value = (a:make-gensym '#:value)
        :when ref
          :collect `(:inflate
                     (,column-name)
                     (lambda (,value)
                       (get-row
                        (sql:select :*
                          (sql:from ,(a:make-keyword (car ref)))
                          (sql:where (:= ,(cadr ref) ,value)))
                        :as ',(car ref))))
        :when func
          :collect `(:inflate (,column-name) ,func)))

(defun %generate-model (name spec)
  `(db:defmodel
       (,name
        (:predicate nil)
        (:copier nil)
        ,@(%generate-inflators spec))
     ,@(mapcar #'car spec)))

(defmacro define-model (name () &body spec)
  (let ((keyword-name (a:make-keyword name)))
    `(progn
       (defmethod make-table ((name (eql ,keyword-name)))
         (db:execute
          (sql:create-table (,name :if-not-exists t)
              ,(%get-table-options spec)
            ,@(%generate-foreign-keys spec))))
       (unless (member ,keyword-name *database-tables*)
         (a:appendf *database-tables* '(,keyword-name)))
       ,(%generate-model name spec))))
