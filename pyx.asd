(asdf:defsystem #:pyx
  :description "A professional-quality game engine with an emphasis on productivity."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx.git")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (#:3b-bmfont
               #:3b-bmfont/json
               #:alexandria
               #:babel
               #:cl-cpus
               #:cl-graph
               #:cl-opengl
               #:closer-mop
               #:doubly-linked-list
               #:fast-io
               #:filtered-functions
               #:global-vars
               #:golden-utils
               #:jsown
               #:lparallel
               #:origin
               #:pngload-fast
               #:sdl2
               #:split-sequence
               #:shadow
               #:uiop
               #:umbra)
  :pathname "src"
  :components
  ((:module "package"
    :components
    ((:file "package")
     (:file "internal")
     (:file "api")
     (:file "nicknames")))

   (:file "config")
   (:file "context")
   (:file "metadata")
   (:file "hardware")

   (:module "util"
    :components
    ((:file "general")
     (:file "thread-pool")
     (:file "live-support")
     (:file "avl-tree")
     (:file "parser")
     (:file "uuid")))

   (:file "opengl")
   (:file "shader")
   (:file "shader-source-basic")
   (:file "shader-source-font")
   (:file "asset")
   (:file "image")
   (:file "image-png")
   (:file "image-hdr")
   (:file "mesh")
   (:file "spritesheet")

   (:module "input"
    :components
    ((:file "data")
     (:file "keyboard")
     (:file "mouse")
     (:file "gamepad")
     (:file "window")
     (:file "button")
     (:file "input")))

   (:module "texture"
    :components
    ((:file "spec")
     (:file "texture")
     (:file "2d")
     (:file "2d-array")
     (:file "cube-map")
     (:file "cube-map-array")))

   (:file "framebuffer")
   (:file "viewport")
   (:file "material")
   (:file "uniform")

   (:module "geometry"
    :components
    ((:file "spec")
     (:file "attribute")
     (:file "group")
     (:file "layout")
     (:file "buffer")
     (:file "geometry")))

   (:file "component")
   (:file "entity-mixin")
   (:file "entity")
   (:file "clock")
   (:file "display")
   (:file "transform")
   (:file "scene")
   (:file "render-pass")
   (:file "render-order")
   (:file "font")

   (:module "collision-detection"
    :components
    ((:file "plan")
     (:file "protocol")
     (:file "shape")
     (:file "shape-sphere")
     (:file "shape-obb")
     (:file "tests")
     (:file "object-picking")
     (:file "system")))

   (:file "animation")
   (:file "animation-states")

   (:module "components"
    :components
    ((:file "node")
     (:file "transform")
     (:file "id")
     (:file "camera")
     (:file "render")
     (:file "mesh")
     (:file "geometry")
     (:file "sprite")
     (:file "collider")
     (:file "animate")
     (:file "font")))

   (:module "prefab"
    :components
    ((:file "common")
     (:file "factory")
     (:file "parser")
     (:file "reference")
     (:file "prefab")))

   (:file "core")))
