(in-package #:pyx.shader)

(define-function default/v ((mesh-attrs mesh-attrs)
                            &uniform
                            (model :mat4)
                            (view :mat4)
                            (proj :mat4))
  (with-slots (mesh/pos mesh/color mesh/uv1) mesh-attrs
    (values (* proj view model (vec4 mesh/pos 1))
            mesh/color
            mesh/uv1)))

(define-function default/f ((color :vec4)
                            (uv :vec2)
                            &uniform
                            (sampler :sampler-2d))
  (texture sampler uv))

(define-shader default ()
  (:vertex (default/v mesh-attrs))
  (:fragment (default/f :vec4 :vec2)))
