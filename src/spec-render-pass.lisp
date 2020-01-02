(in-package #:pyx)

(defclass render-pass-spec ()
  ((%name :reader name
          :initarg :name)
   (%framebuffer :reader framebuffer)
   (%clear-color :reader clear-color)
   (%clear-buffers :reader clear-buffers)))

(define-event-handler :recompile :render-pass recompile-render-pass)

(defun recompile-render-pass (name)
  (declare (ignore name)))

(defun update-render-pass-spec (name framebuffer clear-color clear-buffers)
  (with-slots (%framebuffer %clear-color %clear-buffers)
      (meta :render-passes name)
    (when (and framebuffer (not (meta :framebuffers framebuffer)))
      (error "Framebuffer ~s not defined." framebuffer))
    (setf %framebuffer framebuffer
          %clear-color (or clear-color (v4:vec 0f0 0f0 0f0 1f0))
          %clear-buffers (mapcar
                          (lambda (x)
                            (a:format-symbol :keyword "~a-BUFFER" x))
                          (or clear-buffers '(:color :depth))))
    (enqueue :recompile (list :render-pass name))))

(defun make-render-pass-spec (name framebuffer clear-color clear-buffers)
  (let ((spec (make-instance 'render-pass-spec :name name)))
    (setf (meta :render-passes name) spec)
    (update-render-pass-spec name framebuffer clear-color clear-buffers)
    spec))

(defmacro define-render-pass (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key framebuffer clear-color clear-buffers) (car body)
    `(progn
       (unless (meta :render-passes)
         (setf (meta :render-passes) (u:dict #'eq)))
       (if (meta :render-passes ',name)
           (update-render-pass-spec
            ',name ',framebuffer ,clear-color ',clear-buffers)
           (make-render-pass-spec
            ',name ',framebuffer ,clear-color ',clear-buffers)))))

(define-render-pass :default ()
  (:clear-color (v4:vec 0f0 0f0 0f0 1f0)
   :clear-buffers (:color :depth)))
