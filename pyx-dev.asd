(asdf:defsystem #:pyx-dev
  :description "For internal development use of Pyx."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx.git")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (#:cl-renderdoc
               #:printv
               #:pyx
               #:pyx-examples)
  :pathname "src/dev"
  :serial t
  :components
  ((:file "dev")))
