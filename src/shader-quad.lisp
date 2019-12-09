(in-package #:pyx.shader)

(defun quad/v ((mesh-attrs mesh-attrs)
               &uniforms
               (model :mat4)
               (view :mat4)
               (proj :mat4))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (values (vec4 (.xy mesh/pos) 0 1)
            (.xy mesh/uv1))))

(defun quad/f ((uv :vec2)
               &uniforms
               (sampler :sampler-2d))
  (texture sampler (vec2 (.x uv) (- 1 (.y uv)))))

(define-shader quad ()
  (:vertex (quad/v mesh-attrs))
  (:fragment (quad/f :vec2)))
