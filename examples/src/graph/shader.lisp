(in-package #:pyx.examples.shader)

(defun graph/frag ((uv :vec2)
                   &uniforms
                   (time :float))
  (let* ((dim (vec2 (1+ (sin time)) (+ 2 (sin time))))
         (uv (+ (* uv (- (.y dim) (.x dim)))
                (vec2 (.x dim) -0.5))))
    (umbra.graphing:graph
     (lambda ((x :float))
       (* (sin (* x x x)) (sin x)))
     (* 4 uv)
     (vec4 0 1 0 0.5)
     (vec4 1 1 1 0.02)
     10)))

(define-shader graph ()
  (:vertex (pyx.shader:full-quad/vert mesh-attrs))
  (:fragment (graph/frag :vec2)))
