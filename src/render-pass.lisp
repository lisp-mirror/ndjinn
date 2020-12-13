(in-package #:ndjinn)

(defstruct (render-pass
            (:constructor %make-render-pass)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (framebuffer nil :type symbol)
  (clear-color (v4:vec 0 0 0 1) :type v4:vec)
  (clear-buffers nil :type list))

(defun find-render-pass (name)
  (u:href =meta/render-passes= name))

(defun update-render-pass (name framebuffer-name clear-color clear-buffers)
  (let ((spec (find-render-pass name))
        (buffers (mapcar
                  (lambda (x)
                    (u:format-symbol :keyword "~a-BUFFER" x))
                  (or clear-buffers '(:color :depth)))))
    (when framebuffer-name
      (find-framebuffer-spec framebuffer-name))
    (setf (render-pass-framebuffer spec) framebuffer-name
          (render-pass-clear-color spec) (or clear-color (v4:vec 0 0 0 1))
          (render-pass-clear-buffers spec) buffers)))

(defun make-render-pass (name framebuffer-name clear-color clear-buffers)
  (let ((spec (%make-render-pass :name name)))
    (setf (u:href =meta/render-passes= name) spec)
    (update-render-pass name framebuffer-name clear-color clear-buffers)
    spec))

(defun clear-render-pass (pass)
  (with-framebuffer (find-framebuffer (render-pass-framebuffer pass)) ()
    (v4:with-components ((v (render-pass-clear-color pass)))
      (gl:clear-color vx vy vz vw)
      (apply #'gl:clear (render-pass-clear-buffers pass)))))

(defmacro define-render-pass (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key framebuffer clear-color clear-buffers) (car body)
    `(if (u:href =meta/render-passes= ',name)
         (update-render-pass ',name ',framebuffer ,clear-color ',clear-buffers)
         (make-render-pass ',name ',framebuffer ,clear-color ',clear-buffers))))
