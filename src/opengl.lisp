(in-package #:pyx)

(defun gl/texture-sub-image-2d (id level xoffset yoffset width height format
                                type pixels)
  (gl::with-pixel-array (array type pixels)
    (%gl:texture-sub-image-2d id level xoffset yoffset width height format
                              type array)))

(defun gl/texture-sub-image-3d (id level xoffset yoffset zoffset width height
                                depth format type pixels)
  (gl::with-pixel-array (array type pixels)
    (%gl:texture-sub-image-3d id level xoffset yoffset zoffset width height
                              depth format type array)))

(defun gl/create-framebuffer ()
  (cffi:with-foreign-object (framebuffer '%gl:uint 1)
    (%gl:create-framebuffers 1 framebuffer)
    (cffi:mem-aref framebuffer '%gl:uint 0)))

(defun gl/delete-framebuffer (id)
  (gl:delete-framebuffers (list id)))

(defun gl/named-framebuffer-draw-buffers (id buffers)
  (gl::with-opengl-sequence (seq '%gl:enum buffers)
    (%gl:named-framebuffer-draw-buffers id (length buffers) seq)))

(defun gl/create-renderbuffer ()
  (cffi:with-foreign-object (renderbuffer '%gl:uint 1)
    (%gl:create-renderbuffers 1 renderbuffer)
    (cffi:mem-aref renderbuffer '%gl:uint 0)))

(defmacro with-opengl-state ((enable disable blend-mode depth-mode) &body body)
  `(progn
     (apply #'gl:enable ,enable)
     (apply #'gl:disable ,disable)
     (apply #'gl:blend-func ,blend-mode)
     (gl:depth-func ,depth-mode)
     ,@body
     (apply #'gl:enable ,disable)
     (apply #'gl:disable ,enable)
     (apply #'gl:blend-func +gl-blend-mode+)
     (gl:depth-func +gl-depth-mode+)))
