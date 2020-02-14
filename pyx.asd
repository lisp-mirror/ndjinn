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
   (:file "package-internal")
   (:file "package-api")
   (:file "package-nicknames")
   (:file "config")
   (:file "context")
   (:file "metadata")
   (:file "hardware")
   (:file "util")
   (:file "thread-pool")
   (:file "live-support")
   (:file "avl-tree")
   (:file "binary-parser")
   (:file "uuid")
   (:file "opengl")
   (:file "shader")
   (:file "shader-source-basic")
   (:file "shader-source-font")
   (:file "asset-spec")
   (:file "asset")
   (:file "asset-image")
   (:file "asset-image-png")
   (:file "asset-image-hdr")
   (:file "asset-mesh")
   (:file "asset-spritesheet")
   (:file "input-data")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-gamepad")
   (:file "input-window")
   (:file "input-button")
   (:file "input")
   (:file "texture-spec")
   (:file "texture")
   (:file "texture-2d")
   (:file "texture-2d-array")
   (:file "texture-cube-map")
   (:file "texture-cube-map-array")
   (:file "framebuffer-spec")
   (:file "framebuffer")
   (:file "viewport-spec")
   (:file "viewport")
   (:file "material-spec")
   (:file "material")
   (:file "material-uniforms")
   (:file "geometry-spec")
   (:file "geometry-attribute")
   (:file "geometry-group")
   (:file "geometry-layout")
   (:file "geometry-buffer")
   (:file "geometry")
   (:file "component")
   (:file "entity-mixin")
   (:file "entity")
   (:file "clock")
   (:file "display")
   (:file "transform")
   (:file "scene-spec")
   (:file "scene")
   (:file "render-pass")
   (:file "render-order")
   (:file "font")
   (:file "collision-detection-plan")
   (:file "collision-detection-protocol")
   (:file "collision-detection-shape")
   (:file "collision-detection-shape-sphere")
   (:file "collision-detection-shape-obb")
   (:file "collision-detection-tests")
   (:file "collision-detection-object-picking")
   (:file "collision-detection-system")
   (:file "animation")
   (:file "animation-states")
   (:file "component-node")
   (:file "component-transform")
   (:file "component-id")
   (:file "component-camera")
   (:file "component-render")
   (:file "component-mesh")
   (:file "component-geometry")
   (:file "component-sprite")
   (:file "component-collider")
   (:file "component-animate")
   (:file "component-font")
   (:file "prefab-common")
   (:file "prefab-factory")
   (:file "prefab-parser")
   (:file "prefab-reference")
   (:file "prefab")
   (:file "core")))
