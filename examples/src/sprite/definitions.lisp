(in-package #:pyx-examples)

(pyx:define-texture sprites ()
  (:source (textures sprites)))

(pyx:define-material sprite ()
  (:shader umbra.sprite:sprite
   :uniforms (:sprite.sampler 'sprites
              :opacity 1.0)
   :features (:depth-mode :lequal)))

(pyx:define-animation-sequence sprite ()
  (res:sprite :duration 0.5 :repeat-p t))

(pyx:define-prefab sprite (:add (pyx:sprite pyx:render))
  :sprite/asset '(metadata sprites)
  :render/materials '(sprite))

(pyx:define-prefab planet (:template sprite)
  :transform/scale 2
  :sprite/name "planet11"
  :render/order 'background)

(pyx:define-prefab ship (:template sprite)
  :transform/scale 1.2
  :transform/translate (v3:vec 0 -120)
  :sprite/name "ship29"
  :render/order 'ships
  ((exhaust :template sprite :add (pyx:animate))
   :transform/translate (v3:vec 0 -145)
   :transform/scale (v3:vec 1 0.65 1)
   :sprite/name "exhaust01-01"
   :sprite/frames 8
   :render/order 'ships
   :animate/sequence 'sprite))

(pyx:define-scene sprite ()
  (:draw-order (background ships)
   :sub-trees (examples camera/orthographic planet ship)))
