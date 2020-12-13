(in-package #:ndjinn)

(defstruct (shader-manager
            (:predicate nil)
            (:copier nil))
  (table (u:dict #'eq) :type hash-table)
  (buffer-bindings (u:dict #'equalp) :type hash-table)
  (released-buffer-bindings nil :type list))

(defun initialize-shaders ()
  (unless (display =context=)
    (error "Cannot initialize shaders without an active display."))
  (let* ((table (shadow:load-shaders
                 (lambda (x) (enqueue :recompile (list :shaders x)))))
         (shaders (make-shader-manager :table table)))
    (setf (shaders =context=) shaders)
    (log:debug :ndjinn "Loaded ~d shader programs" (hash-table-count table))))

(on-recompile :shaders data ()
  (shadow:recompile-shaders data)
  (dolist (program data)
    (log:debug :ndjinn "Recompiled shader: ~s" program)))
