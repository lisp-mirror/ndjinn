(in-package #:pyx.examples)

;;; materials

(pyx:define-material world (base)
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

(pyx:define-material world/floor (world)
  (:uniforms (:cell-type 0)))

(pyx:define-material world/wall (world)
  (:uniforms (:cell-type 1)))

;;; prototypes

(pyx:define-prototype tile ()
  (pyx:mesh :file "tiles.glb"))

(pyx:define-prototype tile/floor (tile)
  (pyx:xform :scale (v3:vec 0.5 0.5 0.1))
  (pyx:mesh :name "floor")
  (pyx:render :material 'world/floor))

(pyx:define-prototype tile/wall (tile)
  (pyx:xform :translate (v3:vec 0 0 1.25)
             :scale (v3:vec 0.5 0.5 1.25))
  (pyx:mesh :name "wall")
  (pyx:render :material 'world/wall))

(pyx:define-prototype world ()
  (pyx:xform :scale 40)
  (pyx:world :width 49 :height 49))

;;; prefabs

(pyx:define-prefab world (:template world)
  :world/seed 1
  (floor (:template tile/floor)
         :mesh/instances (@ world :tiles/floor))
  (wall (:template tile/wall)
        :mesh/instances (@ world :tiles/wall)))

(pyx:define-prefab world-scene ()
  (camera (:template camera/isometric)
          :camera/mode :isometric)
  (world (:template (world))))

;;;

(pyx:define-animation translate ()
  (:axis :z
   :offset 1.0))

(defclass animation/translate () ())

(defun foo (bar)
  (eq (class-name (class-of bar))))

(filtered-functions:define-filtered-function on-animation-update (animation)
  (:filters (:update #'foo)))

(defmethod on-animation-update ((animation translate)))

(defmacro define-animation-hook (animation hook-type &body body)
  (let ((hook-types '(:insert :update :finish)))
    (unless (member hook-type hook-types)
      (error "Hook type must be one of: ~{~s~^, ~}" hook-types))
    `(progn
       (defmethod ,(a:format-symbol :pyx "ON-ANIMATION-~a" hook-type)
           ((,animation ,(a:symbolicate '#:animation/ animation)))
         ,@body))))

(define-animation-hook translate :update)

;;;

Suppose I have a wrapper over defclass like:

(define-foo blah ...)

that expands to

(defclass foo/blah () ...)

and I want to write a method that accepts an instance of foo/blah but with a signature like

(defmethod whatever :filter :some-filter ((foo blah)) ...)

and the dispatching function that i write somehow resolves the class foo/blah from the blah specializer

the idea is the user never writes the foo/blah symbol, only the blah part, but they want to define methods specializing on the correct class without knowing what the expansion of define-foo does.
