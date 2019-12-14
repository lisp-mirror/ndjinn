(in-package #:pyx.examples)

;;; materials

(pyx:define-material noise ()
  (:uniforms (:time #'pyx:get-total-time)))

(pyx:define-material noise/perlin-3d (noise)
  (:shader pyx.examples.shader:noise/perlin-3d))

(pyx:define-material noise/perlin-surflet-3d (noise)
  (:shader pyx.examples.shader:noise/perlin-surflet-3d))

(pyx:define-material noise/perlin-improved-3d (noise)
  (:shader pyx.examples.shader:noise/perlin-improved-3d))

(pyx:define-material noise/perlin-4d (noise)
  (:shader pyx.examples.shader:noise/perlin-4d))

(pyx:define-material noise/cellular-3d (noise)
  (:shader pyx.examples.shader:noise/cellular-3d))

(pyx:define-material noise/cellular-fast-3d (noise)
  (:shader pyx.examples.shader:noise/cellular-fast-3d))

(pyx:define-material noise/hermite-3d (noise)
  (:shader pyx.examples.shader:noise/hermite-3d))

(pyx:define-material noise/simplex-perlin-3d (noise)
  (:shader pyx.examples.shader:noise/simplex-perlin-3d))

(pyx:define-material noise/simplex-cellular-3d (noise)
  (:shader pyx.examples.shader:noise/simplex-cellular-3d))

(pyx:define-material noise/simplex-polkadot-3d (noise)
  (:shader pyx.examples.shader:noise/simplex-polkadot-3d))

(pyx:define-material noise/value-3d (noise)
  (:shader pyx.examples.shader:noise/value-3d))

(pyx:define-material noise/value-4d (noise)
  (:shader pyx.examples.shader:noise/value-4d))

(pyx:define-material noise/value-hermite-3d (noise)
  (:shader pyx.examples.shader:noise/value-hermite-3d))

(pyx:define-material noise/value-perlin-3d (noise)
  (:shader pyx.examples.shader:noise/value-perlin-3d))

(pyx:define-material noise/polkadot-3d (noise)
  (:shader pyx.examples.shader:noise/polkadot-3d))

(pyx:define-material noise/polkadot-box-3d (noise)
  (:shader pyx.examples.shader:noise/polkadot-box-3d))

(pyx:define-material noise/cubist-3d (noise)
  (:shader pyx.examples.shader:noise/cubist-3d))

;;; prefabs

(pyx:define-prefab noise-tile (:template quad)
  :xform/scale (v3:vec 90f0 90f0 0f0))

(pyx:define-prefab noise-grid ()
  (perlin-3d (:template noise-tile)
             :xform/translate (v3:vec -540f0 202.5 0f0)
             :render/materials '(noise/perlin-3d))
  (perlin-surflet-3d (:template noise-tile)
                     :xform/translate (v3:vec -325f0 202.5 0f0)
                     :render/materials '(noise/perlin-surflet-3d))
  (perlin-improved-3d (:template noise-tile)
                      :xform/translate (v3:vec -110f0 202.5 0f0)
                      :render/materials '(noise/perlin-improved-3d))
  (perlin-4d (:template noise-tile)
             :xform/translate (v3:vec 110f0 202.5 0f0)
             :render/materials '(noise/perlin-4d))
  (cellular-3d (:template noise-tile)
               :xform/translate (v3:vec 325f0 202.5 0f0)
               :render/materials '(noise/cellular-3d))
  (cellular-fast-3d (:template noise-tile)
                    :xform/translate (v3:vec 540f0 202.5 0f0)
                    :render/materials '(noise/cellular-fast-3d))
  (hermite-3d (:template noise-tile)
              :xform/translate (v3:vec -540f0 -22.5 0f0)
              :render/materials '(noise/hermite-3d))
  (simplex-perlin-3d (:template noise-tile)
                     :xform/translate (v3:vec -325f0 -22.5 0f0)
                     :render/materials '(noise/simplex-perlin-3d))
  (simplex-cellular-3d (:template noise-tile)
                       :xform/translate (v3:vec -110f0 -22.5 0f0)
                       :render/materials '(noise/simplex-cellular-3d))
  (simplex-polkadot-3d (:template noise-tile)
                       :xform/translate (v3:vec 110f0 -22.5 0f0)
                       :render/materials '(noise/simplex-polkadot-3d))
  (value-3d (:template noise-tile)
            :xform/translate (v3:vec 325f0 -22.5 0f0)
            :render/materials '(noise/value-3d))
  (value-4d (:template noise-tile)
            :xform/translate (v3:vec 540f0 -22.5 0f0)
            :render/materials '(noise/value-4d))
  (value-hermite-3d (:template noise-tile)
                    :xform/translate (v3:vec -540f0 -247.5 0f0)
                    :render/materials '(noise/value-hermite-3d))
  (value-perlin-3d (:template noise-tile)
                   :xform/translate (v3:vec -325f0 -247.5 0f0)
                   :render/materials '(noise/value-perlin-3d))
  (polkadot-3d (:template noise-tile)
               :xform/translate (v3:vec -110f0 -247.5 0f0)
               :render/materials '(noise/polkadot-3d))
  (polkadot-box-3d (:template noise-tile)
                   :xform/translate (v3:vec 110f0 -247.5 0f0)
                   :render/materials '(noise/polkadot-box-3d))
  (cubist-3d (:template noise-tile)
             :xform/translate (v3:vec 325f0 -247.5 0f0)
             :render/materials '(noise/cubist-3d)))

;;; scene

(pyx:define-scene noise ()
  (:prefabs (examples camera/orthographic noise-grid)))
