(in-package #:pyx)

(a:define-constant +enabled-capabilities+
    '(:blend :cull-face :depth-test :dither :multisample)
  :test #'equal)

(a:define-constant +disabled-capabilities+
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

(defun make-gpu-object (type)
  (let ((id (case type
              (:texture (gl:gen-texture))
              (:buffer (gl:gen-buffer))
              (:vertex-array (gl:gen-vertex-array))
              (:framebuffer (gl:gen-framebuffer))
              (:render-buffer (gl:gen-renderbuffer)))))
    (push id (u:href (gpu-objects) type))
    id))

(defgeneric free-gpu-object (type id)
  (:method :after (type id)
    (a:deletef (u:href (gpu-objects) type) id)))

(defmethod free-gpu-object ((type (eql :texture)) id)
  (gl:delete-texture id))

(defmethod free-gpu-object ((type (eql :buffer)) id)
  (gl:delete-buffers (list id)))

(defmethod free-gpu-object ((type (eql :vertex-array)) id)
  (gl:delete-vertex-arrays (list id)))

(defmethod free-gpu-object ((type (eql :framebuffer)) id)
  (gl:delete-framebuffers (list id)))

(defmethod free-gpu-object ((type (eql :render-buffer)) id)
  (gl:delete-renderbuffers (list id)))

(defun free-gpu-objects ()
  (gl:delete-textures (u:href (gpu-objects) :texture))
  (gl:delete-buffers (u:href (gpu-objects) :buffer))
  (gl:delete-vertex-arrays (u:href (gpu-objects) :vertex-array))
  (gl:delete-framebuffers (u:href (gpu-objects) :framebuffer))
  (gl:delete-renderbuffers (u:href (gpu-objects) :render-buffer)))

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
