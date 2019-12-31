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

(pyx:define-material test/pass1 ()
  (:output (quad (color))
   :shader pyx.shader:default
   :uniforms (:sampler 'debug)
   :pass pass1))

(pyx:define-material test/pass2 ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'quad/color)
   :features (:disable (:depth-test))
   :pass pass2))

;;; prefabs

(pyx:define-prefab test/pass1 (:add (pyx:mesh pyx:render))
  :xform/rotate (q:orient :local :x math:pi/2)
  :xform/rotate/velocity (math:make-velocity v3:+forward+ (- math:pi/6))
  :xform/scale 15f0
  :mesh/file "helmet.glb"
  :mesh/name "helmet"
  :render/materials '(test/pass1))

(pyx:define-prefab test/pass2 (:add (pyx:mesh pyx:render))
  :mesh/file "plane.glb"
  :mesh/name "plane"
  :render/materials '(test/pass2))

;;; pipeline

(pyx:define-pipeline test ()
  (:passes ((pass1 :clear-color (v4:vec 0.3 0.3 0.3 1.0)
                   :clear-buffers '(:color :depth))
            (pass2 :clear-color (v4:one)
                   :clear-buffers '(:color)))))

;;; scene

(pyx:define-scene test ()
  (:pipeline test
   :prefabs (examples camera/perspective test/pass1 test/pass2)))
