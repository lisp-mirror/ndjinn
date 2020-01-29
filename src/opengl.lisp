(in-package #:%pyx.opengl)

(a:define-constant +enabled+
    '(:blend :cull-face :depth-test :dither :multisample)
  :test #'equal)

(a:define-constant +disabled+
    '(:clip-distance0 :clip-distance1 :clip-distance2 :clip-distance3
      :clip-distance4 :clip-distance5 :clip-distance6 :clip-distance7
      :color-logic-op :debug-output :debug-output-synchronous :depth-clamp
      :framebuffer-srgb :line-smooth :polygon-offset-fill :polygon-offset-line
      :polygon-offset-point :polygon-smooth :primitive-restart
      :primitive-restart-fixed-index :rasterizer-discard
      :sample-alpha-to-coverage :sample-alpha-to-one
      :sample-coverage :sample-shading :sample-mask :scissor-test :stencil-test
      :texture-cube-map-seamless :program-point-size)
  :test #'equal)

(a:define-constant +blend-mode+ '(:src-alpha :one-minus-src-alpha)
  :test #'equal)

(a:define-constant +depth-mode+ :less)

(a:define-constant +polygon-mode+ '(:front-and-back :fill) :test #'equal)

;;; TODO: Remove these when cl-opengl supports them

(defun named-framebuffer-draw-buffers (id buffers)
  (gl::with-opengl-sequence (seq '%gl:enum buffers)
    (%gl:named-framebuffer-draw-buffers id (length buffers) seq)))

(defmacro with-debug-group (name &body body)
  (if (find :pyx.release *features*)
      `(progn ,@body)
      (a:once-only (name)
        `(progn
           (cffi:with-foreign-string (s ,name)
             (%gl:push-debug-group
              :debug-source-application 0 (length ,name) s))
           (unwind-protect (progn ,@body)
             (%gl:pop-debug-group))))))
