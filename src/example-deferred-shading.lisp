(in-package #:pyx.examples)

;;;TODO: This example is not complete yet.

(pyx:define-texture gbuffer/position/normal ()
  (:min-filter :nearest
   :mag-filter :nearest
   :pixel-format :rgb
   :pixel-type :float
   :internal-format :rgb16f
   :generate-mipmaps nil
   :source nil))

(pyx:define-texture gbuffer/albedo/specular ()
  (:min-filter :nearest
   :mag-filter :nearest
   :pixel-format :rgba
   :pixel-type :unsigned-byte
   :internal-format :rgba8
   :generate-mipmaps nil
   :source nil))

(pyx:define-framebuffer gbuffer ()
  (position :buffer (:texture gbuffer/position/normal)
            :point (:color 0))
  (normal :buffer (:texture gbuffer/position/normal)
          :point (:color 1))
  (albedo/specular :buffer (:texture gbuffer/albedo/specular)
                   :point (:color 2)))
