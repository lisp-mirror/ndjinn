(push :pyx.release *features*)
(push :cl-opengl-no-masked-traps *features*)
(push :cl-opengl-no-check-error *features*)

(asdf:defsystem #:net.mfiano.lisp.pyx.deploy
  :description "A system used for deploying a game developed with Pyx."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (#:net.mfiano.lisp.pyx)
  :pathname "src/deploy"
  :serial t
  :components
  ((:file "deploy")))
