(in-package #:%pyx.shader)

(defstruct (shaders (:conc-name nil)
                    (:predicate nil)
                    (:copier nil))
  table
  (buffer-bindings (u:dict #'equalp))
  released-buffer-bindings)

(defun initialize ()
  (unless (ctx:display)
    (error "Cannot initialize shaders without an active display."))
  (let* ((table (shadow:load-shaders
                 (lambda (x) (util::enqueue :recompile (list :shaders x)))))
         (shaders (make-shaders :table table)))
    (setf (ctx:shaders) shaders)))

(defun select-buffer-binding ()
  (let ((id-count (hash-table-count (buffer-bindings (ctx:shaders)))))
    (when (= id-count hw:=max-ssbo-bindings=)
      (error "Cannot create shader buffer. Maximum bindings reached: ~d."
             hw:=max-ssbo-bindings=))
    (or (pop (released-buffer-bindings (ctx:shaders)))
        (1+ id-count))))

(defun release-buffer-binding (key)
  (a:when-let* ((shaders (ctx:shaders))
                (id (u:href (buffer-bindings shaders) key)))
    (remhash key (buffer-bindings shaders))
    (pushnew id (released-buffer-bindings shaders))
    (setf (released-buffer-bindings shaders)
          (sort (copy-seq (released-buffer-bindings shaders)) #'<))))

(util::on-recompile :shaders data ()
  (shadow:recompile-shaders data))

(setf (fdefinition 'write-shader-buffer) #'shadow:write-buffer-path)

(setf (fdefinition 'read-shader-buffer) #'shadow:read-buffer-path)

(defun make-shader-buffer (key block-id shader)
  (let ((binding (select-buffer-binding)))
    (setf (u:href (buffer-bindings (ctx:shaders)) key) binding)
    (shadow:create-block-alias :buffer block-id shader key)
    (shadow:bind-block key binding)
    (shadow:create-buffer key key)
    (shadow:bind-buffer key binding)))

(defgeneric update-shader-buffer (object))

(defun delete-shader-buffer (key)
  (release-buffer-binding key)
  (shadow:delete-buffer key)
  (shadow:unbind-block key))
