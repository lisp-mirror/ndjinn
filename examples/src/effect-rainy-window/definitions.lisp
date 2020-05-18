(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-texture effect/rainy-window ()
  (:source (textures city)))

(pyx:define-material effect/rainy-window ()
  (:shader shader:window-rain
   :features (:disable (:multisample))
   :uniforms (:blur 7.5
              :speed 0.24
              :zoom 0.8
              :time (pyx:as-uniform 'pyx:get-running-time)
              :res (pyx:as-uniform 'pyx:get-viewport-dimensions)
              :sampler 'effect/rainy-window)))

(pyx:define-prefab effect/rainy-window (:template quad)
  :render/materials '(effect/rainy-window))

(pyx:define-scene effect/rainy-window ()
  (:sub-trees (examples camera/orthographic effect/rainy-window)))
