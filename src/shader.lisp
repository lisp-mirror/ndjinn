(in-package #:pyx)

(defclass shaders ()
  ((%programs :reader programs
              :initarg :programs)
   (%hooks :reader hooks
           :initform (queues:make-queue :simple-cqueue))))

(defun initialize-shaders (game-state)
  (with-slots (%shaders) game-state
    (let ((programs (shadow:load-shaders
                     (lambda (x) (queues:qpush (hooks %shaders) x)))))
      (setf %shaders (make-instance 'shaders :programs programs)))))

(defun recompile-shaders (game-state)
  (loop :with queue = (hooks (shaders game-state))
        :for (programs . found-p) = (multiple-value-list (queues:qpop queue))
        :while found-p
        :when programs
          :do (shadow:recompile-shaders programs)
              (log:info :pyx "Recompiled shader programs: ~{~s~^, ~}"
                        programs)))
