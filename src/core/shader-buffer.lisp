(in-package #:net.mfiano.lisp.pyx)

(defun select-shader-buffer-binding ()
  (let* ((shaders (shaders =context=))
         (id-count (hash-table-count (shader-manager-buffer-bindings shaders)))
         (max-bindings (hardware-info-max-ssbo-bindings
                        (hardware-info =context=))))
    (when (= id-count max-bindings)
      (error "Cannot create shader buffer. Maximum bindings reached: ~d."
             max-bindings))
    (or (pop (shader-manager-released-buffer-bindings shaders))
        (1+ id-count))))

(defun release-shader-buffer-binding (key)
  (u:when-let* ((shaders (shaders =context=))
                (bindings (shader-manager-buffer-bindings shaders))
                (id (u:href bindings key)))
    (remhash key bindings)
    (pushnew id (shader-manager-released-buffer-bindings shaders))
    (setf (shader-manager-released-buffer-bindings shaders)
          (sort (copy-seq (shader-manager-released-buffer-bindings shaders))
                #'<))))

(defun write-shader-buffer (key path value)
  (shadow:write-buffer-path key path value))

(defun read-shader-buffer (key path)
  (shadow:read-buffer-path key path))

(defun make-shader-buffer (key block-id shader)
  (u:mvlet ((major minor (get-opengl-version)))
    (unless (and (>= major 4)
                 (>= minor 3))
      (error "Shader buffer usage requires OpenGL version 4.3 or greater, but ~
              game was configured for OpenGL version ~d.~d" major minor)))
  (let ((bindings (shader-manager-buffer-bindings (shaders =context=)))
        (binding (select-shader-buffer-binding)))
    (setf (u:href bindings key) binding)
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
  (let* ((bindings (shader-manager-buffer-bindings (shaders =context=)))
         (binding (u:href bindings key)))
    (shadow:bind-block key binding)
    (shadow:bind-buffer key binding)))

(defun unbind-shader-buffer (key)
  (shadow:unbind-buffer key)
  (shadow:unbind-block key))

(defmacro with-shader-buffers ((&rest keys) &body body)
  (u:with-gensyms (table)
    (let ((key-syms (mapcar (lambda (x) (list (u:make-gensym x) x)) keys)))
      `(let ((,table (shader-manager-buffer-bindings (shaders =context=)))
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
