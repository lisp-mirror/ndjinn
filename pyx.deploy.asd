(push :pyx.release *features*)
(push :cl-opengl-no-check-error *features*)

(asdf:defsystem #:pyx.deploy
  :description "A system used for deploying a game developed with Pyx."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx.git")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (#:pyx)
  :pathname "src"
  :serial t
  :components
  ((:file "deploy")))
