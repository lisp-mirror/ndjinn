(in-package #:pyx-examples)

(pyx:define-component mouse-input () ())

(pyx:define-entity-hook :pre-render (entity mouse-input)
  (u:mvlet ((res (pyx:get-viewport-dimensions))
            (x y (pyx:get-mouse-position)))
    (when (pyx:on-button-enabled :mouse :left)
      (pyx:set-uniforms entity :mouse (v2:/ (v2:vec x y) res)))))

(pyx:define-material effect/ocean-waves ()
  (:shader shader:effect/ocean-waves
   :uniforms (:time (pyx:as-uniform 'pyx:get-running-time)
              :res (pyx:as-uniform 'pyx:get-viewport-dimensions)
              :mouse (v2:vec))))

(pyx:define-prefab effect/ocean-waves (:template quad :add (mouse-input))
  :render/materials '(effect/ocean-waves))

(pyx:define-scene effect/ocean-waves ()
  (:sub-trees (examples camera/orthographic effect/ocean-waves)))
