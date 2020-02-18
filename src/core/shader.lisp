(in-package #:pyx)

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
  (a:when-let* ((shaders (shaders))
                (id (u:href (buffer-bindings shaders) key)))
    (remhash key (buffer-bindings shaders))
    (pushnew id (released-buffer-bindings shaders))
    (setf (released-buffer-bindings shaders)
          (sort (copy-seq (released-buffer-bindings shaders)) #'<))))

(on-recompile :shaders data ()
  (shadow:recompile-shaders data))

(setf (fdefinition 'write-shader-buffer) #'shadow:write-buffer-path)

(setf (fdefinition 'read-shader-buffer) #'shadow:read-buffer-path)

(defun make-shader-buffer (key block-id shader)
  (let ((binding (select-shader-buffer-binding)))
    (setf (u:href (buffer-bindings (shaders)) key) binding)
    (shadow:create-block-alias :buffer block-id shader key)
    (shadow:bind-block key binding)
    (shadow:create-buffer key key)
    (shadow:bind-buffer key binding)))

(defgeneric update-shader-buffer (object))

(defun delete-shader-buffer (key)
  (release-shader-buffer-binding key)
  (shadow:delete-buffer key)
  (shadow:unbind-block key))
