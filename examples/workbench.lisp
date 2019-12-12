(in-package #:pyx.examples)

;;;; This is not a real example. It just serves as a workbench for engine
;;;; development.

;;; textures

(pyx:define-texture quad/color ()
  (:min-filter :linear
   :mag-filter :linear
   :pixel-format :rgb
   :pixel-type :unsigned-byte
   :internal-format :rgb8
   :generate-mipmaps nil
   :source nil))

;;; framebuffers

(pyx:define-framebuffer quad ()
  (color :buffer (:texture quad/color)
         :point (:color 0))
  (depth/stencil :buffer :render-buffer
                 :point :depth/stencil))

;;; materials

(pyx:define-material quad/pass1 ()
  (:output (quad (color))
   :shader pyx.shader:quad
   :uniforms (:sampler 'quad/color)
   :features (:enable (:depth-test))
   :pass quad))

(pyx:define-material test/quad/pass2 ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'quad/color)
   :features (:disable (:depth-test))))

;;; prefabs

(pyx:define-prefab test ()
  (camera (:template camera/orthographic))
  (quad (:add (pyx:mesh pyx:render))
        :xform/scale 50
        :mesh/file "plane.glb"
        :mesh/name "plane"
        :render/materials '(quad/pass1)))

;;; pipeline

(pyx:define-pipeline test ()
  (:passes ((quad :clear-color (v4:vec 0.3 0.3 0.3 1.0)
                  :clear-buffers '(:color :depth)))))

;;; scene

(pyx:define-scene test ()
  (:pipeline test
   :prefabs (test)))
