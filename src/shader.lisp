(in-package #:pyx)

(defun initialize-shaders ()
  (with-slots (%shaders) (database *state*)
    (setf %shaders (shadow:load-shaders
                    (lambda (x)
                      (enqueue :recompile (list :shaders x)))))))

(defun recompile-shaders (program-names)
  (shadow:recompile-shaders program-names)
  (log:info :pyx "Recompiled shader programs: ~{~s~^, ~}" program-names))

(defun select-shader-buffer-binding ()
  (with-slots (%database) *state*
    (let* ((table (shader-buffer-bindings %database))
           (id-count (hash-table-count table))
           (max-bindings (get-gpu-parameter :max-shader-storage-buffer-bindings)))
      (when (= id-count max-bindings)
        (error "Cannot create shader buffer. Maximum bindings reached: ~d."
               max-bindings))
      (or (pop (released-shader-buffer-bindings %database))
          (1+ id-count)))))

(defun release-shader-buffer-binding (key)
  (with-slots (%shader-buffer-bindings %released-shader-buffer-bindings)
      (database *state*)
    (a:when-let ((id (u:href %shader-buffer-bindings key)))
      (remhash key %shader-buffer-bindings)
      (pushnew id %released-shader-buffer-bindings)
      (setf %released-shader-buffer-bindings
            (sort (copy-seq %released-shader-buffer-bindings) #'<)))))

(defun make-shader-buffer (key block-id shader)
  (with-slots (%shader-buffer-bindings) (database *state*)
    (let ((binding (select-shader-buffer-binding)))
      (setf (u:href %shader-buffer-bindings key) binding)
      (shadow:create-block-alias :buffer block-id shader key)
      (shadow:bind-block key binding)
      (shadow:create-buffer key key)
      (shadow:bind-buffer key binding))))

(defgeneric update-shader-buffer (object))

(defun delete-shader-buffer (key)
  (release-shader-buffer-binding key)
  (shadow:delete-buffer key))
