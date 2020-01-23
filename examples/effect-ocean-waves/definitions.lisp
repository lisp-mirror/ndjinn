(in-package #:pyx.examples)

;;; components

(pyx:define-component mouse-input () ())

(pyx:define-hook :update (entity mouse-input)
  (u:mvlet ((res (pyx:get-viewport-dimensions))
            (x y (pyx:get-mouse-position)))
    (when (pyx:input-enabled-p :mouse :left)
      (pyx:set-uniforms entity :mouse (v2:/ (v2:vec x y) res)))))

;;; materials

(pyx:define-material effect/ocean-waves ()
  (:shader pyx.examples.shader:effect/ocean-waves
   :uniforms (:time 'pyx:get-total-time
              :res 'pyx:get-viewport-dimensions
              :mouse (v2:zero))))

;;; prefabs

(pyx:define-prefab effect/ocean-waves (:template quad
                                       :add (mouse-input))
  :render/materials '(effect/ocean-waves))

;;; scene

(pyx:define-scene effect/ocean-waves ()
  (:sub-trees (examples camera/orthographic effect/ocean-waves)))
