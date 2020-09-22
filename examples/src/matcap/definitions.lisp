(in-package #:net.mfiano.lisp.pyx.examples)

(pyx:define-texture matcaps (:2d-array)
  (:source ((res::matcaps res:matcap-0029)
            (res::matcaps res:matcap-0064)
            (res::matcaps res:matcap-0048)
            (res::matcaps res:matcap-0255)
            (res::matcaps res:matcap-0115)
            (res::matcaps res:matcap-0108)
            (res::matcaps res:matcap-0108)
            (res::matcaps res:matcap-0108))))

(pyx:define-texture matcap-id-map (:2d)
  (:min-filter :nearest
   :mag-filter :nearest
   :source (mesh-textures ship-material-map)))

(pyx:define-material matcap ()
  (:shader shader:matcap
   :uniforms (:matcaps 'matcaps
              :id-map 'matcap-id-map
              :normal-matrix 'pyx:resolve-normal-matrix)))

(pyx:define-prefab matcap-mesh (:add (pyx:mesh pyx:render))
  :transform/rotate/velocity (math:make-velocity v3:+back+ 1.2f0)
  :transform/rotate (q:orient :local :x 2.4f0 :y 0.4f0 :z 2.4f0)
  :transform/scale 5
  :mesh/asset '(meshes ships)
  :mesh/name "Ship_001"
  :render/materials '(matcap))

(pyx:define-scene matcap ()
  (:sub-trees (examples camera/perspective matcap-mesh)))

;; testing

(pyx:define-context matcap ()
  (:scene matcap))
