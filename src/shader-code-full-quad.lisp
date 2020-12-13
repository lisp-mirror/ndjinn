(in-package #:ndjinn.shader)

(defun full-quad/vertex ((pos2 :vec3)
                         (uv :vec2)
                         &uniforms
                         (model :mat4)
                         (view :mat4)
                         (proj :mat4))
  (values (vec4 (* (.xy pos2) 2) 0 1)
          uv))

(defun full-quad-no-uv/vertex ((pos2 :vec3)
                               &uniforms
                               (model :mat4)
                               (view :mat4)
                               (proj :mat4))
  (vec4 (* (.xy pos2) 2) 0 1))

(define-shader full-quad ()
  (:vertex (full-quad/vertex :vec3 :vec2))
  (:fragment (quad/fragment :vec2)))
