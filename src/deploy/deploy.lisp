(in-package #:net.mfiano.lisp.pyx)

(glob:define-global-var =release= nil)

(defun deploy/check-features ()
  (unless (and (find :pyx.release *features*)
               (find :cl-opengl-no-check-error *features*))
    (error "You can only deploy by depending on the pyx.deploy system.")))

(defun deploy/setup ()
  (setf =release= t)
  (log:stop log:*global-controller*)
  #+sbcl
  (progn
    (sb-ext:disable-debugger)
    (sb-ext:gc :full t)))

(defun deploy/dump-binary (file top-level)
  (let ((deploy-options
          #+windows '(:application-type :gui)
          #-windows nil))
    (apply #'sb-ext:save-lisp-and-die
           file
           :toplevel top-level
           :executable t
           :save-runtime-options t
           :compression 9
           deploy-options)))

(defun deploy (file context-name &rest options)
  #+sbcl
  (let ((top-level (lambda () (apply #'start context-name options))))
    (deploy/check-features)
    (deploy/setup)
    (deploy/dump-binary file top-level))
  #-sbcl
  (error "Deployment is only supported on SBCL."))
