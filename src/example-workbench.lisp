(in-package #:pyx.examples)

;;; This is not a real example. It just serves as a workbench for engine
;;; development.

(pyx:define-prototype world ()
  (pyx:xform :scale 50)
  (pyx:world :options nil
             :level 1))

(pyx:define-prototype tile ()
  (pyx:mesh :file "tiles.glb"))

(pyx:define-prototype tile/floor (tile)
  (pyx:xform :scale (v3:vec 0.5 0.5 0.1))
  (pyx:mesh :name "floor")
  (pyx:render :material 'world/floor))

(pyx:define-prototype tile/wall (tile)
  (pyx:xform :translate (v3:vec 0 0 0.75)
             :scale (v3:vec 0.5 0.5 0.75))
  (pyx:mesh :name "wall")
  (pyx:render :material 'world/wall))

;; (define-prefab foo ()
;;   ((world world
;;           :xform/scale 50)
;;    ((floors tile/floor
;;             :mesh/instances (u:href (pyx:world/cell-counts world) :floor)))
;;    ((walls tile/wall
;;            :mesh/instances (u:href (pyx:world/cell-counts world) :wall)))))
