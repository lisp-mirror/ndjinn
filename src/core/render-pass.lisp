(in-package #:pyx)

(defclass render-pass-spec ()
  ((%name :reader name
          :initarg :name)
   (%framebuffer :accessor framebuffer)
   (%clear-color :accessor clear-color)
   (%clear-buffers :accessor clear-buffers)))

(defun find-render-pass-spec (name)
  (u:href =render-passes= name))

(defun update-render-pass-spec (name framebuffer-name clear-color clear-buffers)
  (let ((spec (find-render-pass-spec name)))
    (when framebuffer-name
      (find-framebuffer-spec framebuffer-name))
    (setf (framebuffer spec) framebuffer-name
          (clear-color spec) (or clear-color (v4:vec 0 0 0 1))
          (clear-buffers spec) (mapcar
                                (lambda (x)
                                  (a:format-symbol :keyword "~a-BUFFER" x))
                                (or clear-buffers '(:color :depth))))))

(defun make-render-pass-spec (name framebuffer-name clear-color clear-buffers)
  (let ((spec (make-instance 'render-pass-spec :name name)))
    (setf (u:href =render-passes= name) spec)
    (update-render-pass-spec name framebuffer-name clear-color clear-buffers)
    spec))

(defun clear-render-pass (pass)
  (with-framebuffer (find-framebuffer (framebuffer pass)) ()
    (v4:with-components ((v (clear-color pass)))
      (gl:clear-color vx vy vz vw)
      (apply #'gl:clear (clear-buffers pass)))))

(defmacro define-render-pass (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key framebuffer clear-color clear-buffers) (car body)
    `(if (u:href =render-passes= ',name)
         (update-render-pass-spec ',name ',framebuffer ,clear-color
                                  ',clear-buffers)
         (make-render-pass-spec ',name ',framebuffer ,clear-color
                                ',clear-buffers))))

(define-render-pass default ()
  (:clear-color (v4:vec 0 0 0 1)
   :clear-buffers (:color :depth)))
