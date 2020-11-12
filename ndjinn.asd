(push :cl-opengl-no-masked-traps *features*)

(asdf:defsystem #:ndjinn
  :description "A professional-quality game engine with an emphasis on productivity."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://github.com/ndjinn/ndjinn")
  :bug-tracker "https://github.com/ndjinn/ndjinn/issues"
  :encoding :utf-8
  :depends-on (#:3b-bmfont
               #:3b-bmfont/json
               #:babel
               #:cl-cpus
               #:cl-digraph
               #:cl-opengl
               #:cl-slug
               #:closer-mop
               #:fast-io
               #:filtered-functions
               #:global-vars
               #:jsown
               #:lparallel
               #:net.mfiano.lisp.algae
               #:net.mfiano.lisp.golden-utils
               #:net.mfiano.lisp.origin
               #:net.mfiano.lisp.shadow
               #:net.mfiano.lisp.umbra
               #:pngload
               #:printv
               #:queues.priority-cqueue
               #:sdl2
               #:split-sequence
               #:trivial-garbage
               #:uiop
               #:verbose)
  :pathname "src"
  :components
  ((:file "package")
   (:module "base"
    :components
    ((:file "general")
     (:file "context")
     (:file "profile")
     (:file "metadata")
     (:file "config")
     (:file "logging")
     (:file "hardware")
     (:file "thread-pool")
     (:file "live-support")
     (:file "parser")
     (:file "asset")))
   (:module "input"
    :components
    ((:file "data")
     (:file "transition")
     (:file "keyboard")
     (:file "mouse")
     (:file "gamepad")
     (:file "window")
     (:file "event")))
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
     (:file "shape-box")
     (:file "tests")
     (:file "object-picking")
     (:file "system")))
   (:module "shader"
    :components
    ((:file "default")
     (:file "quad")
     (:file "full-quad")
     (:file "mesh")
     (:file "collider")
     (:file "font")
     (:file "matcap")))
   (:module "core"
    :components
    ((:file "opengl")
     (:file "shader-program")
     (:file "shader-buffer")
     (:file "image")
     (:file "image-png")
     (:file "image-hdr")
     (:file "gltf")
     (:file "spritesheet")
     (:file "framebuffer")
     (:file "viewport")
     (:file "material")
     (:file "uniform")
     (:file "flows")
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
     (:file "curve")))
   (:module "prefab"
    :components
    ((:file "common")
     (:file "factory")
     (:file "parser")
     (:file "reference")
     (:file "prefab")))
   (:module "components"
    :components
    ((:file "node")
     (:file "transform")
     (:file "id")
     (:file "debug")
     (:file "camera")
     (:file "render")
     (:file "mesh")
     (:file "geometry")
     (:file "sprite")
     (:file "collider")
     (:file "font")
     (:file "mouse-control")
     (:file "animate")
     (:file "curve")))
   (:file "definitions")
   (:file "ndjinn")))