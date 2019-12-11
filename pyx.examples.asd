(asdf:defsystem #:pyx.examples
  :description "Example scenes for the Pyx game engine."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx.git")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (#:pyx)
  :pathname "src/examples"
  :serial t
  :components
  ((:file "package")
   (:file "shaders")
   (:file "common")
   (:file "graph")
   (:file "mesh")
   (:file "noise")
   (:file "sprite")
   (:file "texture")
   (:file "world")
   (:file "deferred-shading")
   #++(:file "workbench")))
