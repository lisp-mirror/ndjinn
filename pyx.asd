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
               #:golden-utils
               #:origin
               #:queues.simple-cqueue
               #:sdl2
               #:sdl2-image
               #:shadow
               #:uiop
               #:umbra
               #:verbose)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "clock")
   (:file "interactive-development")
   (:file "host")
   (:file "display")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-window")
   (:file "input")
   (:file "scene-graph")
   (:file "transform")
   (:file "shader")
   (:file "entity")
   (:file "game-state")))
