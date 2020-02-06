(in-package #:pyx.examples)

(pyx:define-component skybox ()
  ((%geometry :accessor geometry
              :initarg :skybox/geometry
              :initform nil)))

(pyx:define-entity-hook :attach (entity skybox)
  (setf geometry (pyx:make-geometry geometry)))

(pyx:define-entity-hook :render (entity skybox)
  (pyx:draw-geometry geometry 1))

(pyx:define-material skybox ()
  (:shader shader:skybox
   :uniforms (:sampler 'ext:sky)
   :features (:enable (:texture-cube-map-seamless)
              :depth-mode :lequal)))

(pyx:define-geometry-layout skybox ()
  (:data (:format interleaved)
         (position :type float :count 3)))

(pyx:define-geometry skybox ()
  (:layout skybox
   :primitive :triangles
   :vertex-count 36
   :buffers
   (:data ((-1 1 -1) (-1 -1 -1) (1 -1 -1) (1 -1 -1) (1 1 -1) (-1 1 -1)
           (-1 -1 1) (-1 -1 -1) (-1 1 -1) (-1 1 -1) (-1 1 1) (-1 -1 1)
           (1 -1 -1) (1 -1 1) (1 1 1) (1 1 1) (1 1 -1) (1 -1 -1)
           (-1 -1 1) (-1 1 1) (1 1 1) (1 1 1) (1 -1 1) (-1 -1 1)
           (-1 1 -1) (1 1 -1) (1 1 1) (1 1 1) (-1 1 1) (-1 1 -1)
           (-1 -1 -1) (-1 -1  1) (1 -1 -1) (1 -1 -1) (-1 -1 1) (1 -1 1)))))

(pyx:define-prefab skybox ()
  ((camera :template camera/perspective)
   :camera/translate-view nil)
  ((skybox :add (pyx:render skybox))
   :skybox/geometry 'skybox
   :render/materials '(skybox))
  ((mesh :template mesh-carousel)
   :transform/translate (v3:vec 0 0 -50)
   :render/materials '(mesh)))

(pyx:define-scene skybox ()
  (:sub-trees (examples skybox)))
