(in-package #:pyx.examples)

;;; components

(pyx:define-component scene-switcher ()
  ((%scenes :accessor scenes
            :initform nil)))

(pyx:define-hook :update (entity scene-switcher)
  (unless scenes
    (setf scenes (remove-if
                  (lambda (x)
                    (or (not (eq (symbol-package x)
                                 (find-package :pyx.examples)))
                        (eq x 'examples)))
                  (sort (u:hash-keys (pyx:meta :scenes)) #'string<))))
  (let ((index (or (position (pyx:get-scene-name) scenes) 0)))
    (cond
      ((pyx:on-button-exit :key :up)
       (decf index))
      ((pyx:on-button-exit :key :down)
       (incf index))
      ((pyx:on-button-enter :key :escape)
       (pyx:stop-engine))
      ((pyx:on-button-enter :mouse :left)
       (pyx::pick-entity)))
    (pyx:switch-scene (elt scenes (mod index (length scenes))))))

;;; prefabs

(pyx:define-prefab camera (:add (pyx:camera))
  :camera/debug t)

(pyx:define-prefab camera/perspective (:template camera)
  :camera/debug-speed 0.1
  :xform/translate (v3:vec 0 0 50))

(pyx:define-prefab camera/orthographic (:template camera)
  :camera/debug-speed 4
  :xform/translate (v3:vec 0 0 1)
  :camera/mode :orthographic
  :camera/clip-near 0
  :camera/clip-far 16)

(pyx:define-prefab camera/isometric (:template camera)
  :camera/debug-speed 4
  :xform/translate (v3:vec 0 0 1)
  :camera/mode :isometric
  :camera/clip-near -1000
  :camera/clip-far 1000)

(pyx:define-prefab quad (:add (pyx:mesh pyx:render))
  :mesh/file "plane.glb"
  :mesh/name "plane"
  :render/materials '(pyx:quad))

(pyx:define-prefab full-quad (:template quad)
  :render/materials '(pyx:full-quad))

(pyx:define-prefab examples (:add (scene-switcher)))

;;; scene

(pyx:define-scene examples ()
  (:sub-trees ((examples examples))))
