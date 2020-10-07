(in-package #:net.mfiano.lisp.pyx.shader)

(defun quad/vert ((pos2 :vec3)
                  (uv :vec2)
                  &uniforms
                  (model :mat4)
                  (view :mat4)
                  (proj :mat4))
  (values (* proj view model (vec4 (.xy pos2) 0 1))
          uv))

(defun quad/frag ((uv :vec2)
                  &uniforms
                  (sampler :sampler-2d))
  (texture sampler uv))

(define-shader quad ()
  (:vertex (quad/vert :vec3 :vec2))
  (:fragment (quad/frag :vec2)))
