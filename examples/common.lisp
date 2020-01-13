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
      ((pyx:input-exit-p :key :up)
       (decf index))
      ((pyx:input-exit-p :key :down)
       (incf index))
      ((pyx:input-enter-p :key :escape)
       (pyx:stop-engine)))
    (pyx:switch-scene (elt scenes (mod index (length scenes))))))

;;; prefabs

(pyx:define-prefab camera (:add (pyx:camera))
  :camera/debug t)

(pyx:define-prefab camera/perspective (:template camera)
  :xform/translate (v3:vec 0f0 0f0 50f0))

(pyx:define-prefab camera/orthographic (:template camera)
  :xform/translate (v3:vec 0f0 0f0 1f0)
  :camera/mode :orthographic
  :camera/clip-near 0f0
  :camera/clip-far 16f0)

(pyx:define-prefab camera/isometric (:template camera)
  :xform/translate (v3:vec 0f0 0f0 1f0)
  :camera/mode :isometric
  :camera/clip-near -1000f0
  :camera/clip-far 1000f0)

(pyx:define-prefab quad (:add (pyx:mesh pyx:render))
  :mesh/file "plane.glb"
  :mesh/name "plane"
  :render/materials '(pyx:quad))

(pyx:define-prefab examples (:add (scene-switcher)))

;;; scene

(pyx:define-scene examples ()
  (:prefabs (examples)))
