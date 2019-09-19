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
               #:cl-opengl
               #:closer-mop
               #:dynamic-mixins
               #:fast-io
               #:golden-utils
               #:jsown
               #:origin
               #:pngload
               #:queues.simple-cqueue
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
   (:file "interactive-development")
   (:file "binary-parser")
   (:file "config")
   (:file "clock")
   (:file "display")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-window")
   (:file "input")
   (:file "transform-state")
   (:file "shader")
   (:file "asset")
   (:file "texture")
   (:file "spritesheet")
   (:file "mesh")                       ; TODO: make into component
   (:file "entity")
   (:file "component")               ; TODO: add before/after to toposort mixins
   (:file "component-node")
   (:file "component-render")
   (:file "component-xform")
   (:file "component-camera")
   (:file "component-sprite")
   (:file "state")))
