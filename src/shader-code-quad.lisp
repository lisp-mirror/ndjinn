(in-package #:ndjinn.shader)

(defun quad/vertex ((pos :vec3)
                    (uv :vec2)
                    &uniforms
                    (model :mat4)
                    (view :mat4)
                    (proj :mat4))
  (values (* proj view model (vec4 (.xy pos) 0 1))
          uv))

(defun quad/fragment ((uv :vec2)
                      &uniforms
                      (sampler :sampler-2d))
  (texture sampler uv))

(define-shader quad ()
  (:vertex (quad/vertex :vec3 :vec2))
  (:fragment (quad/fragment :vec2)))
