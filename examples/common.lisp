(in-package #:pyx.examples)

(pyx:define-component scene-switcher ()
  ((%scenes :accessor scenes
            :initform nil)))

(pyx:define-entity-hook :update (entity scene-switcher)
  (unless scenes
    (setf scenes (remove 'examples (pyx:get-registered-scene-names
                                    :pyx.examples))))
  (let ((index (or (position (pyx:get-scene-name) scenes) 0)))
    (cond
      ((pyx:on-button-exit :key :up)
       (decf index))
      ((pyx:on-button-exit :key :down)
       (incf index))
      ((pyx:on-button-enter :key :escape)
       (pyx:stop-engine))
      ((pyx:on-button-enter :mouse :left)
       (pyx:pick-entity)))
    (pyx:switch-scene (elt scenes (mod index (length scenes))))))

(pyx:define-prefab camera (:add (pyx:camera))
  :camera/debug t)

(pyx:define-prefab camera/perspective (:template camera)
  :camera/debug-speed 0.1
  :transform/translate (v3:vec 0 0 50))

(pyx:define-prefab camera/orthographic (:template camera)
  :camera/debug-speed 4
  :transform/translate (v3:vec 0 0 1)
  :camera/mode :orthographic
  :camera/clip-near 0
  :camera/clip-far 16)

(pyx:define-prefab camera/isometric (:template camera)
  :transform/translate (v3:vec 0 0 1)
  :camera/debug-speed 4
  :camera/mode :isometric
  :camera/clip-near -1000
  :camera/clip-far 1000)

(pyx:define-prefab quad (:add (pyx:mesh pyx:render))
  :mesh/file "plane.glb"
  :mesh/name "plane"
  :render/materials '(ext:quad))

(pyx:define-prefab full-quad (:template quad)
  :render/materials '(ext:full-quad))

(pyx:define-prefab examples (:add (scene-switcher)))

(pyx:define-scene examples ()
  (:sub-trees ((examples examples))))
