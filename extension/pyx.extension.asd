(asdf:defsystem #:pyx.extension
  :description "Builtin extensions for Pyx."
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
  ((:file "package")
   (:file "textures")
   (:file "meshes")
   (:file "data")))
