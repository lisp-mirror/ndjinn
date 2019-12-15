(in-package #:pyx.examples)

;;; textures

(pyx:define-texture debug ()
  (:source "debug.png"))

;;; materials

(pyx:define-material quad ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'debug)))

;;; components

(pyx:define-component scene-switcher ()
  ((%scenes :accessor scenes
            :initform nil)))

(pyx:define-hook :entity-create (entity scene-switcher)
  (v:info :pyx "Switched to scene ~a." (pyx:get-current-scene-name)))

(pyx:define-hook :entity-update (entity scene-switcher)
  (unless scenes
    (setf scenes (remove-if
                  (lambda (x)
                    (or (not (eq (symbol-package x)
                                 (find-package :pyx.examples)))
                        (eq x 'examples)))
                  (sort (u:hash-keys (pyx:meta :scenes)) #'string<))))
  (let ((index (or (position (pyx:get-current-scene-name) scenes) 0)))
    (when (= (random 300) 57)
      (error "foo"))
    (cond
      ((pyx:input-exit-p :key :up)
       (decf index))
      ((pyx:input-exit-p :key :down)
       (incf index))
      ((pyx:input-enter-p :key :escape)
       (pyx:stop)))
    (pyx:switch-scene (elt scenes (mod index (length scenes))))))

;;; prefabs

(pyx:define-prefab camera/perspective (:add (pyx:camera))
  :xform/translate (v3:vec 0f0 0f0 50f0))

(pyx:define-prefab camera/orthographic (:add (pyx:camera))
  :xform/translate (v3:vec 0f0 0f0 1f0)
  :camera/mode :orthographic
  :camera/clip-near 0f0
  :camera/clip-far 16f0)

(pyx:define-prefab camera/isometric (:add (pyx:camera))
  :xform/translate (v3:vec 0f0 0f0 1f0)
  :camera/mode :isometric
  :camera/clip-near -1000f0
  :camera/clip-far 1000f0)

(pyx:define-prefab quad (:add (pyx:mesh pyx:render))
  :mesh/file "plane.glb"
  :mesh/name "plane"
  :render/materials '(quad))

(pyx:define-prefab examples (:add (scene-switcher)))

;;; scene

(pyx:define-scene examples ()
  (:prefabs (examples)))
