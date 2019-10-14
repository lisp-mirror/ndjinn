(in-package #:pyx.shader)

(defun default/v ((mesh-attrs mesh-attrs)
                  &uniforms
                  (model :mat4)
                  (view :mat4)
                  (proj :mat4))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (values (* proj view model (vec4 mesh/pos 1))
            mesh/uv1)))

(defun default/f ((uv :vec2)
                  &uniforms
                  (sampler :sampler-2d))
  (texture sampler uv))

(define-shader default ()
  (:vertex (default/v mesh-attrs))
  (:fragment (default/f :vec2)))
