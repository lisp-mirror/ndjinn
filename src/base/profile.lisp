(in-package #:net.mfiano.lisp.pyx)

(defvar *profile* nil)
(defvar *profile-frames* 1800)
(glob:define-global-var =profile-frame-counter= 0)

(defmacro with-profile (&body body)
  (let ((packages (remove-if-not
                   (lambda (x)
                     (or (u:string-starts-with-p x "NET.MFIANO")
                         (string= x "SDL2")
                         (string= x "CL-OPENGL")))
                   (mapcar #'package-name (list-all-packages)))))
    `(if *profile*
         (unwind-protect
              (progn
                (setf =profile-frame-counter= 0)
                (sb-profile:unprofile)
                (sb-profile:profile ,@packages)
                (submit-job :profile
                            (let ((target *profile-frames*))
                              (lambda ()
                                (loop :until (>= =profile-frame-counter= target)
                                      :finally (stop-engine)))))
                ,@body)
           (sb-profile:report)
           (sb-profile:unprofile)
           (sb-profile:reset)
           (setf =profile-frame-counter= 0))
         (progn ,@body))))
