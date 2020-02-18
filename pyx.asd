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
  ((:file "package")
   (:module "base"
    :components
    ((:file "general")
     (:file "config")
     (:file "metadata")
     (:file "hardware")
     (:file "thread-pool")
     (:file "live-support")
     (:file "avl-tree")
     (:file "parser")
     (:file "uuid")
     (:file "asset")
     (:file "context")))
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
   (:module "geometry"
    :components
    ((:file "spec")
     (:file "attribute")
     (:file "group")
     (:file "layout")
     (:file "buffer")
     (:file "geometry")))
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
   (:module "shader"
    :components
    ((:file "basic")
     (:file "font")))
   (:module "components"
    :depends-on ("core")
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
     (:file "font")
     (:file "mouse-control")))
   (:module "prefab"
    :components
    ((:file "common")
     (:file "factory")
     (:file "parser")
     (:file "reference")
     (:file "prefab")))
   (:module "core"
    :components
    ((:file "opengl")
     (:file "shader")
     (:file "image")
     (:file "image-png")
     (:file "image-hdr")
     (:file "gltf")
     (:file "spritesheet")
     (:file "framebuffer")
     (:file "viewport")
     (:file "material")
     (:file "uniform")
     (:file "component")
     (:file "entity-mixin")
     (:file "entity")
     (:file "clock")
     (:file "display")
     (:file "transform")
     (:file "free-look-state")
     (:file "scene")
     (:file "render-pass")
     (:file "render-order")
     (:file "font")
     (:file "animation")
     (:file "animation-states")))
   (:file "pyx")))
