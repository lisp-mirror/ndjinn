(in-package #:pyx.examples)

;;; materials

(pyx:define-material mesh ()
  (:shader pyx.shader:default
   :uniforms (:sampler 'debug)))

;;; prefabs

(pyx:define-prefab mesh-example ()
  (camera (:template camera)
          :xform/translate (v3:vec 0 0 50)
          :camera/zoom 1
          :camera/fov-y 90)
  (mesh (:add (pyx:mesh pyx:render))
        :xform/rotate/inc (v3:vec 0 0.01 0)
        :xform/scale 30
        :mesh/file "sphere.glb"
        :mesh/name "sphere"
        :render/materials '(mesh)))

;;; scenes

(pyx:define-scene mesh ()
  (:prefabs (mesh-example)))
