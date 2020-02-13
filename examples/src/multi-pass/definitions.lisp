(in-package #:pyx-examples)

(pyx:define-texture multi-pass/color ()
  (:min-filter :linear
   :pixel-format :rgba
   :internal-format :rgba8
   :generate-mipmaps nil))

(pyx:define-framebuffer multi-pass ()
  (color :buffer (:texture multi-pass/color)
         :point (:color 0))
  (depth/stencil :buffer :render-buffer
                 :point :depth/stencil))

(pyx:define-material multi-pass/pass1 (mesh)
  (:output (multi-pass (color))
   :pass multi-pass/pass1))

(pyx:define-material multi-pass/pass2 ()
  (:shader shader:multi-pass
   :uniforms (:sampler 'multi-pass/color)
   :features (:disable (:depth-test))
   :pass multi-pass/pass2))

(pyx:define-prefab multi-pass/mesh (:template mesh-carousel)
  :render/materials '(multi-pass/pass1))

(pyx:define-prefab multi-pass/quad (:template quad)
  :render/materials '(multi-pass/pass2))

(pyx:define-render-pass multi-pass/pass1 ()
  (:framebuffer multi-pass
   :clear-color (v4:vec 0.8 0.8 0.8 1)))

(pyx:define-render-pass multi-pass/pass2 ()
  (:clear-buffers (:color)))

(pyx:define-scene multi-pass ()
  (:passes (multi-pass/pass1 multi-pass/pass2)
   :sub-trees (examples camera/perspective multi-pass/mesh multi-pass/quad)))
