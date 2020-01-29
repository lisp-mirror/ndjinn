(asdf:defsystem #:pyx.examples
  :description "Example scenes for the Pyx game engine."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx.git")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (#:dungen
               #:pyx)
  :pathname "examples"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "workbench")
   (:file "deferred-shading/definitions")
   (:file "effect-kaleidoscope/shader")
   (:file "effect-kaleidoscope/definitions")
   (:file "effect-truchet/shader")
   (:file "effect-truchet/definitions")
   (:file "effect-toroidal-trip/shader")
   (:file "effect-toroidal-trip/definitions")
   (:file "effect-rainy-window/definitions")
   (:file "effect-ocean-waves/shader")
   (:file "effect-ocean-waves/definitions")
   (:file "graph/shader")
   (:file "graph/definitions")
   (:file "mesh/definitions")
   (:file "noise/shaders")
   (:file "noise/definitions")
   (:file "sprite/definitions")
   (:file "texture/definitions")
   (:file "world/shader")
   (:file "world/component-world")
   (:file "world/definitions")
   (:file "multi-pass/shader")
   (:file "multi-pass/definitions")
   (:file "collider/definitions")
   (:file "viewports/definitions")
   (:file "font/definitions")))
