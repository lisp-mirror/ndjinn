;; (push :cl-opengl-no-masked-traps *features*)

(asdf:defsystem #:ndjinn
  :description "A professional-quality game engine with an emphasis on productivity."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://github.com/ndjinn/ndjinn")
  :bug-tracker "https://github.com/ndjinn/ndjinn/issues"
  :encoding :utf-8
  :depends-on (#:3b-bmfont
               #:3b-bmfont/json
               #:algae
               #:babel
               #:cl-cpus
               #:cl-digraph
               #:cl-opengl
               #:cl-slug
               #:closer-mop
               #:fast-io
               #:filtered-functions
               #:global-vars
               #:golden-utils
               #:jsown
               #:lparallel
               #:origin
               #:pngload
               #:printv
               #:sdl2
               #:sdl2-mixer
               #:shadow
               #:split-sequence
               #:trivial-garbage
               #:uiop
               #:umbra
               #:verbose)
  :pathname "src"
  :components
  ((:file "package")
   (:file "general")
   (:file "context")
   (:file "profile")
   (:file "metadata")
   (:file "config")
   (:file "logging")
   (:file "hardware")
   (:file "thread-pool")
   (:file "clock")
   (:file "live-support")
   (:file "parser")
   (:file "opengl")
   (:file "asset")
   (:file "display")
   (:file "input-data")
   (:file "input-transition")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-gamepad")
   (:file "input-window")
   (:file "input-event")
   (:file "image")
   (:file "image-png")
   (:file "image-hdr")
   (:file "texture-spec")
   (:file "texture")
   (:file "texture-2d")
   (:file "texture-2d-array")
   (:file "texture-cube-map")
   (:file "texture-cube-map-array")
   (:file "geometry-layout-spec")
   (:file "geometry-spec")
   (:file "geometry-attribute")
   (:file "geometry-group")
   (:file "geometry-buffer")
   (:file "geometry")
   (:file "gltf")
   (:file "delayed-work")
   (:file "shader-program")
   (:file "shader-buffer")
   (:file "shader-code-default")
   (:file "shader-code-quad")
   (:file "shader-code-full-quad")
   (:file "shader-code-mesh")
   (:file "shader-code-collider")
   (:file "shader-code-font")
   (:file "shader-code-matcap")
   (:file "scene-structure")
   (:file "component")
   (:file "entity-mixin")
   (:file "entity")
   (:file "viewport")
   (:file "transform-state")
   (:file "free-look-state")
   (:file "framebuffer")
   (:file "scene")
   (:file "material")
   (:file "uniform")
   (:file "render-pass")
   (:file "render-order")
   (:file "font")
   (:file "spritesheet")
   (:file "animation")
   (:file "collision-plan")
   (:file "collision-protocol")
   (:file "collision-shape")
   (:file "collision-shape-sphere")
   (:file "collision-shape-box")
   (:file "collision-test")
   (:file "collision-system")
   (:file "collision-picking")
   (:file "audio")
   (:file "curve-spec")
   (:file "prefab-common")
   (:file "prefab-factory")
   (:file "prefab-parser")
   (:file "prefab-reference")
   (:file "prefab")
   (:file "component-type-node")
   (:file "component-type-transform")
   (:file "component-type-id")
   (:file "component-type-debug")
   (:file "component-type-camera")
   (:file "component-type-render")
   (:file "component-type-mesh")
   (:file "component-type-geometry")
   (:file "component-type-sprite")
   (:file "component-type-collider")
   (:file "component-type-font")
   (:file "component-type-mouse-control")
   (:file "component-type-animate")
   (:file "component-type-curve")
   (:file "definitions")
   (:file "engine")))
