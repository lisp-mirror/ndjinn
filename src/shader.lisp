(in-package #:pyx)

(defclass shaders ()
  ((%programs :reader programs
              :initarg :programs)
   (%hooks :reader hooks
           :initform (queues:make-queue :simple-cqueue))))

(defun initialize-shaders ()
  (with-slots (%shaders) *state*
    (let ((programs (shadow:load-shaders
                     (lambda (x) (queues:qpush (hooks %shaders) x)))))
      (setf %shaders (make-instance 'shaders :programs programs)))))

(defun recompile-shaders ()
  (loop :with queue = (hooks (shaders *state*))
        :for (programs . found-p) = (multiple-value-list (queues:qpop queue))
        :while found-p
        :when programs
          :do (shadow:recompile-shaders programs)
              (log:info :pyx "Recompiled shader programs: ~{~s~^, ~}"
                        programs)))

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
