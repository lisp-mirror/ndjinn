(in-package #:net.mfiano.lisp.pyx)

(defclass shaders ()
  ((%table :reader table
           :initarg :table)
   (%buffer-bindings :reader buffer-bindings
                     :initform (u:dict #'equalp))
   (%released-buffer-bindings :accessor released-buffer-bindings
                              :initform nil)))

(defun initialize-shaders ()
  (unless (display)
    (error "Cannot initialize shaders without an active display."))
  (let* ((table (shadow:load-shaders
                 (lambda (x) (enqueue :recompile (list :shaders x)))))
         (shaders (make-instance 'shaders :table table)))
    (setf (shaders) shaders)))

(on-recompile :shaders data ()
  (shadow:recompile-shaders data))
