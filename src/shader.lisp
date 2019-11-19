(in-package #:pyx)

(defun initialize-shaders ()
  (with-slots (%shaders) (database *state*)
    (setf %shaders (shadow:load-shaders
                    (lambda (x)
                      (enqueue :recompile (list :shaders x)))))))

(defun recompile-shaders (program-names)
  (shadow:recompile-shaders program-names)
  (log:info :pyx "Recompiled shader programs: ~{~s~^, ~}" program-names))

(defun make-shader-buffer (block-id name shader)
  (with-slots (%shader-buffer-bindings) (database *state*)
    (incf %shader-buffer-bindings)
    (shadow:create-block-alias :buffer block-id shader name)
    (shadow:bind-block name %shader-buffer-bindings)
    (shadow:create-buffer name name)
    (shadow:bind-buffer name %shader-buffer-bindings)))

(defgeneric update-shader-buffer (object buffer))

(defun delete-shader-buffer (name)
  (shadow:delete-buffer name))
