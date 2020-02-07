(in-package #:%pyx.render)

(defstruct (pass-spec (:constructor %make-pass-spec)
                      (:conc-name nil)
                      (:predicate nil)
                      (:copier nil))
  name
  framebuffer
  clear-color
  clear-buffers)

(defun find-pass-spec (name)
  (u:href meta:=render-passes= name))

(defun collect-passes-using-framebuffer (framebuffer-name)
  (let (passes)
    (dolist (pass-name (scene:passes (ctx:current-scene)))
      (let ((pass (find-pass-spec pass-name)))
        (when (eq (framebuffer pass) framebuffer-name)
          (push pass-name passes))))
    passes))

(defun update-pass-spec (name framebuffer-name clear-color clear-buffers)
  (let ((spec (find-pass-spec name)))
    (when framebuffer-name
      (fb:find-spec framebuffer-name))
    (setf (framebuffer spec) framebuffer-name
          (clear-color spec) (or clear-color (v4:vec 0 0 0 1))
          (clear-buffers spec) (mapcar
                                (lambda (x)
                                  (a:format-symbol :keyword "~a-BUFFER" x))
                                (or clear-buffers '(:color :depth))))))

(defun make-pass-spec (name framebuffer-name clear-color clear-buffers)
  (let ((spec (%make-pass-spec :name name)))
    (setf (u:href meta:=render-passes= name) spec)
    (update-pass-spec name framebuffer-name clear-color clear-buffers)
    spec))

(defun clear-pass (pass)
  (fb:with-framebuffer (fb:find (framebuffer pass)) ()
    (v4:with-components ((v (clear-color pass)))
      (gl:clear-color vx vy vz vw)
      (apply #'gl:clear (clear-buffers pass)))))

(defun delete-pass (pass-name)
  (let ((scene (ctx:current-scene)))
    (when (find pass-name (scene:passes scene))
      (clear-pass (find-pass-spec pass-name))
      (a:deletef (scene:passes scene) pass-name))))

;;; Public API

(defmacro define-render-pass (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key framebuffer clear-color clear-buffers) (car body)
    `(if (u:href meta:=render-passes= ',name)
         (update-pass-spec ',name ',framebuffer ,clear-color ',clear-buffers)
         (make-pass-spec ',name ',framebuffer ,clear-color ',clear-buffers))))

(define-render-pass :default ()
  (:clear-color (v4:vec 0 0 0 1)
   :clear-buffers (:color :depth)))
