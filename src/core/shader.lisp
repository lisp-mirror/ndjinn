(in-package #:net.mfiano.lisp.pyx)

(defclass shaders ()
  ((%table :reader table
           :initarg :table)
   (%buffer-bindings :reader buffer-bindings
                     :initform (u:dict #'equalp))
   (%released-buffer-bindings :accessor released-buffer-bindings
                              :initform nil)))

(defun initialize-shaders ()
  (unless (display)
    (error "Cannot initialize shaders without an active display."))
  (let* ((table (shadow:load-shaders
                 (lambda (x) (enqueue :recompile (list :shaders x)))))
         (shaders (make-instance 'shaders :table table)))
    (setf (shaders) shaders)))

(defun select-shader-buffer-binding ()
  (let ((id-count (hash-table-count (buffer-bindings (shaders)))))
    (when (= id-count =max-ssbo-bindings=)
      (error "Cannot create shader buffer. Maximum bindings reached: ~d."
             =max-ssbo-bindings=))
    (or (pop (released-buffer-bindings (shaders)))
        (1+ id-count))))

(defun release-shader-buffer-binding (key)
  (u:when-let* ((shaders (shaders))
                (id (u:href (buffer-bindings shaders) key)))
    (remhash key (buffer-bindings shaders))
    (pushnew id (released-buffer-bindings shaders))
    (setf (released-buffer-bindings shaders)
          (sort (copy-seq (released-buffer-bindings shaders)) #'<))))

(on-recompile :shaders data ()
  (shadow:recompile-shaders data))

(defun write-shader-buffer (key path value)
  (shadow:write-buffer-path key path value))

(defun read-shader-buffer (key path)
  (shadow:read-buffer-path key path))

(defun make-shader-buffer (key block-id shader)
  (let ((binding (select-shader-buffer-binding)))
    (setf (u:href (buffer-bindings (shaders)) key) binding)
    (shadow:create-block-alias :buffer block-id shader key)
    (shadow:bind-block key binding)
    (shadow:create-buffer key key)
    (shadow:bind-buffer key binding)
    binding))

(defgeneric update-shader-buffer (object))

(defun delete-shader-buffer (key)
  (release-shader-buffer-binding key)
  (shadow::clear-buffer key)
  (shadow:delete-buffer key)
  (shadow:unbind-block key))

(defun clear-shader-buffer (key)
  (shadow:clear-buffer key))

(defun bind-shader-buffer (key)
  (let ((binding (u:href (buffer-bindings (shaders)) key)))
    (shadow:bind-block key binding)
    (shadow:bind-buffer key binding)))

(defun unbind-shader-buffer (key)
  (shadow:unbind-buffer key)
  (shadow:unbind-block key))

(defmacro with-shader-buffers ((&rest keys) &body body)
  (u:with-gensyms (table)
    (let ((key-syms (mapcar (lambda (x) (list (u:make-gensym x) x)) keys)))
      `(let ((,table (buffer-bindings (shaders)))
             ,@key-syms)
         ,@(mapcar
            (lambda (x)
              `(shadow:bind-block ,(car x) (u:href ,table ,(car x))))
            key-syms)
         ,@body
         ,@(mapcar
            (lambda (x)
              `(unbind-shader-buffer ,(car x)))
            key-syms)))))
