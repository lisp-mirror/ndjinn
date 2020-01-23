(in-package #:pyx.examples)

;;; textures

(pyx:define-texture effect/rainy-window ()
  (:source "city.png"))

;;; materials

(pyx:define-material effect/rainy-window ()
  (:shader umbra.effects:window-rain
   :features (:disable (:multisample))
   :uniforms (:blur 7.5f0
              :speed 0.24f0
              :zoom 0.75f0
              :time 'pyx:get-total-time
              :res 'pyx:get-viewport-dimensions
              :sampler 'effect/rainy-window)))

;;; prefabs

(pyx:define-prefab effect/rainy-window (:template quad)
  :render/materials '(effect/rainy-window))

;;; scene

(pyx:define-scene effect/rainy-window ()
  (:sub-trees (examples camera/orthographic effect/rainy-window)))
