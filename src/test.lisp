(in-package #:pyx)

(define-material base ()
  (:uniforms (:model (m4:id)
              :view (m4:id)
              :proj (m4:id))))

(define-material sprite ()
  (:shader umbra.sprite:sprite))

(define-material world (base)
  (:shader pyx.shader:world
   :uniforms (:light.position (v3:vec 0.1 0.25 -1)
              :light.ambient (v4:vec 0.01 0.01 0.01 0.01)
              :light.diffuse (v4:vec 0.5 0.5 0.5 0.5)
              :light.specular (v4:vec 0.2 0.2 0.2 0.2)
              :material.ambient (v4:one)
              :material.diffuse (v4:one)
              :material.specular (v4:one)
              :material.shininess 10
              :opacity 1.0)))

(define-material world/floor (world)
  (:uniforms (:cell-type 0)))

(define-material world/wall (world)
  (:uniforms (:cell-type 1)))

(defun make-world (level &rest args)
  (let ((world (make-entity (world)
                 :xform/scale 50
                 :xform/rotate/inc (v3:vec 0 0 0.1)
                 :world/options args
                 :world/level level)))
    (make-entity (render mesh)
      :node/parent world
      :xform/scale (v3:vec 0.5 0.5 0.1)
      :render/material 'world/floor
      :mesh/file "floor.glb"
      :mesh/instances (u:href (world/cell-counts world) :floor))
    (make-entity (render mesh)
      :node/parent world
      :xform/translate (v3:vec 0 0 0.75)
      :xform/scale (v3:vec 0.5 0.5 0.75)
      :render/material 'world/wall
      :mesh/file "wall.glb"
      :mesh/instances (u:href (world/cell-counts world) :wall))))

(defun test ()
  (make-entity (camera)
    :camera/mode :isometric)
  (make-world 1 :width 49 :height 49 :seed 1))

(defun test2 ()
  (make-entity (camera))
  (make-entity (render sprite)
    :xform/rotate/inc (v3:vec 0 0 0.1)
    :sprite/file "sprites.png"
    :sprite/name "planet01"
    :render/material 'sprite))
