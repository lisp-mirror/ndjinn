(asdf:defsystem #:pyx
  :description ""
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx.git")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (#:alexandria
               #:cl-graph
               #:cl-opengl
               #:closer-mop
               #:dungen
               #:fast-io
               #:golden-utils
               #:jsown
               #:lparallel
               #:origin
               #:pngload-fast
               #:sdl2
               #:shadow
               #:uiop
               #:umbra
               #:verbose)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "metadata")
   (:file "threading")
   (:file "interactive-development")
   (:file "uuid")
   (:file "binary-parser")
   (:file "config")
   (:file "clock")
   (:file "database")
   (:file "queued-events")
   (:file "display")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-gamepad")
   (:file "input-window")
   (:file "input")
   (:file "transform-state")
   (:file "uniform")
   (:file "shader")
   (:file "shader-default")
   (:file "shader-world")
   (:file "material-spec")
   (:file "material")
   (:file "asset")
   (:file "image")
   (:file "texture-spec")
   (:file "texture")
   (:file "spritesheet")
   (:file "gltf")
   (:file "mixin")
   (:file "entity")
   (:file "component")
   (:file "component-identify")
   (:file "component-node")
   (:file "component-render")
   (:file "component-xform")
   (:file "component-camera")
   (:file "component-world")
   (:file "component-sprite")
   (:file "component-mesh")
   (:file "state")
   (:file "test")))
