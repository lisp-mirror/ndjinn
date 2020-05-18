(in-package #:net.mfiano.lisp.pyx)

(defvar *deployed-p* nil)

(defun deploy/check-features ()
  (unless (and (find :pyx.release *features*)
               (find :cl-opengl-no-check-error *features*))
    (error "You can only deploy using the supplied shell script.")))

(defun deploy/setup ()
  (setf *deployed-p* t)
  (sb-ext:disable-debugger)
  (sb-ext:gc :full t))

(defun deploy/dump-binary (file scene &rest options)
  (let ((deploy-options
          #+windows '(:application-type :gui)
          #-windows nil))
    (apply #'sb-ext:save-lisp-and-die
           file
           :toplevel (lambda ()
                       (apply #'start scene :allow-other-keys t options))
           :executable t
           :save-runtime-options t
           :compression 9
           deploy-options)))

(defun deploy (file scene &rest options)
  (deploy/check-features)
  (deploy/setup)
  #+sbcl (deploy/dump-binary file scene options)
  #-sbcl (error "Deployment is only supported on SBCL."))
