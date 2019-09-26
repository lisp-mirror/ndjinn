(in-package #:pyx.shader)

(define-struct world-data
  (width :uint :accessor width)
  (height :uint :accessor height)
  (cells/floor (:ivec2 75625) :accessor cells/floor)
  (cells/wall (:ivec2 75625) :accessor cells/wall)
  (cells/door/v (:ivec2 75625) :accessor cells/door/v)
  (cells/door/h (:ivec2 75625) :accessor cells/door/h))

(define-function get-cell-coords ((world world-data)
                                  (cell-type :int))
  (with-slots (cells/floor cells/wall cells/door/v cells/door/h) world
    (case cell-type
      (1 (aref cells/wall gl-instance-id))
      (2 (aref cells/door/v gl-instance-id))
      (3 (aref cells/door/h gl-instance-id))
      (otherwise (aref cells/floor gl-instance-id)))))

(define-function world/v ((mesh-attrs mesh-attrs)
                          &uniform
                          (model :mat4)
                          (view :mat4)
                          (proj :mat4)
                          (cell-type :int)
                          (world world-data :ssbo :std-430))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (with-slots (width height) world
      (let* ((world-size (vec2 width height))
             (coords (get-cell-coords world cell-type))
             (offset (vec3 (- (* coords 2) world-size) 0))
             (cell-pos (vec3 (* model (vec4 (+ offset mesh/pos) 1)))))
        (values (* proj view (vec4 cell-pos 1))
                mesh/uv1)))))

(define-function world/f ((uv :vec2))
  (vec4 1 0 0 1))

(define-shader world ()
  (:vertex (world/v mesh-attrs))
  (:fragment (world/f :vec2)))
