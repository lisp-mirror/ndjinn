(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-component scene-switcher ()
  ((%scene-switcher/scenes :accessor scene-switcher/scenes
                           :initform nil)))

(pyx:define-entity-hook :update (entity scene-switcher)
  (let ((scenes (remove 'examples (pyx:get-registered-scene-names
                                   :net.mfiano.lisp.pyx.examples))))
    (unless (scene-switcher/scenes entity)
      (setf (scene-switcher/scenes entity) scenes)
      (when (eq (pyx:get-scene-name) 'examples)
        (pyx:switch-scene (first scenes))))
    (let ((index (or (position (pyx:get-scene-name) scenes) 0)))
      (cond
        ((pyx:on-button-exit :key :up)
         (decf index)
         (pyx:switch-scene (elt scenes (mod index (length scenes)))))
        ((pyx:on-button-exit :key :down)
         (incf index)
         (pyx:switch-scene (elt scenes (mod index (length scenes)))))
        ((pyx:on-button-enter :key :escape)
         (pyx:stop-engine))
        ((pyx:on-button-enter :mouse :left)
         (pyx:pick-entity))))))

(pyx:define-component mouse-input () ())

(pyx:define-entity-hook :pre-render (entity mouse-input)
  (u:mvlet ((res (pyx:get-viewport-size))
            (x y (pyx:get-mouse-position)))
    (when (pyx:on-button-enabled :mouse :left)
      (pyx:set-uniforms entity :mouse (v2:/ (v2:vec x y) res)))))
