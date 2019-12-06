(asdf:defsystem #:pyx
  :description "A professional-quality game engine with an emphasis on productivity."
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
               #:doubly-linked-list
               #:dungen
               #:fast-io
               #:filtered-functions
               #:golden-utils
               #:jsown
               #:lparallel
               #:origin
               #:pngload-fast
               #:sdl2
               #:shadow
               #:trivial-features
               #:uiop
               #:umbra
               #:verbose)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "constants")
   (:file "metadata")
   (:file "threading")
   (:file "interactive-development")
   (:file "opengl")
   (:file "uuid")
   (:file "binary-parser")
   (:file "config")
   (:file "clock")
   (:file "database")
   (:file "queued-events")
   (:file "display")
   (:file "gpu-query")
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
   (:file "framebuffer-spec")
   (:file "framebuffer")
   (:file "material-spec")
   (:file "material")
   (:file "asset")
   (:file "image")
   (:file "texture-spec")
   (:file "texture")
   (:file "spritesheet")
   (:file "grid")
   (:file "grid-quad")
   (:file "grid-quad-4-way")
   (:file "grid-quad-8-way")
   (:file "grid-hex")
   (:file "grid-hex-rows")
   (:file "grid-hex-columns")
   (:file "gltf")
   (:file "geometry-attribute")
   (:file "geometry-group")
   (:file "geometry-buffer")
   (:file "geometry-layout")
   (:file "geometry")
   (:file "mixin")
   (:file "entity")
   (:file "entity-query")
   (:file "component")
   (:file "component-animate")
   (:file "component-identify")
   (:file "component-node")
   (:file "component-render")
   (:file "component-xform")
   (:file "component-camera")
   (:file "component-world")
   (:file "component-sprite")
   (:file "component-mesh")
   (:file "component-group")
   (:file "prefab-common")
   (:file "prefab-factory")
   (:file "prefab-parser")
   (:file "prefab-reference")
   (:file "prefab")
   (:file "group-spec")
   (:file "animation")
   (:file "animation-states")
   (:file "scene-spec")
   (:file "state")
   (:file "example-common")
   (:file "example-sprite")
   (:file "example-world")
   (:file "example-deferred-shading")
   (:file "example-workbench")))
