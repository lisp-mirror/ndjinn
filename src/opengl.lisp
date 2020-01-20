(in-package #:pyx)

(defun gl/named-framebuffer-draw-buffers (id buffers)
  (gl::with-opengl-sequence (seq '%gl:enum buffers)
    (%gl:named-framebuffer-draw-buffers id (length buffers) seq)))

(defmacro with-debug-group (name &body body)
  (a:once-only (name)
    `(progn
       (cffi:with-foreign-string (s ,name)
         (%gl:push-debug-group :debug-source-application 0 (length ,name) s))
       (unwind-protect (progn ,@body)
         (%gl:pop-debug-group)))))
