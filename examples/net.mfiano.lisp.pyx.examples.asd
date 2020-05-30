(asdf:defsystem #:net.mfiano.lisp.pyx.examples
  :description "Example scenes for the Pyx game engine."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (#:net.mfiano.lisp.dungen
               #:net.mfiano.lisp.pyx
               #:net.mfiano.lisp.pyx.resources)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "base/assets")
   (:file "base/components")
   (:file "base/definitions")
   (:file "deferred-shading/definitions")
   (:file "effect-kaleidoscope/shader")
   (:file "effect-kaleidoscope/definitions")
   (:file "effect-truchet/shader")
   (:file "effect-truchet/definitions")
   (:file "effect-toroidal-trip/shader")
   (:file "effect-toroidal-trip/definitions")
   (:file "effect-rainy-window/shader")
   (:file "effect-rainy-window/definitions")
   (:file "effect-ocean-waves/shader")
   (:file "effect-ocean-waves/definitions")
   (:file "graph/shader")
   (:file "graph/definitions")
   (:file "mesh-carousel/shader")
   (:file "mesh-carousel/definitions")
   (:file "noise/shaders")
   (:file "noise/definitions")
   (:file "sprite/definitions")
   (:file "texture/definitions")
   (:file "world/shader")
   (:file "world/component-world")
   (:file "world/definitions")
   (:file "multi-pass/shader")
   (:file "multi-pass/definitions")
   (:file "colliders1/definitions")
   (:file "colliders2/definitions")
   (:file "viewports/definitions")
   (:file "font/definitions")
   (:file "skybox/shader")
   (:file "skybox/definitions")))
