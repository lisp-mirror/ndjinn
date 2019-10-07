(in-package #:pyx)

(defun initialize-shaders ()
  (with-slots (%shaders) *state*
    (setf %shaders (shadow:load-shaders
                    (lambda (x)
                      (enqueue :recompile (list :shader x)))))))

(defun recompile-shaders (program-names)
  (shadow:recompile-shaders program-names)
  (log:info :pyx "Recompiled shader programs: ~{~s~^, ~}" program-names))

(defun make-shader-buffer (name shader)
  (symbol-macrolet ((binding (u:href (cache *state*) :shader-buffers)))
    (a:with-gensyms (alias)
      (unless binding
        (setf binding 0))
      (incf binding)
      (shadow:create-block-alias :buffer name shader alias)
      (shadow:bind-block alias binding)
      (shadow:create-buffer name alias)
      (shadow:bind-buffer name binding))))

(defgeneric update-shader-buffer (object buffer &key))
