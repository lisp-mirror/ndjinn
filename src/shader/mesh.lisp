(in-package #:net.mfiano.lisp.pyx.shader)

(defun mesh/vert ((mesh-attrs mesh-attrs)
                  &uniforms
                  (model :mat4)
                  (view :mat4)
                  (proj :mat4))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (values (* proj view model (vec4 mesh/pos 1))
            mesh/uv1)))

(defun mesh/frag ((uv :vec2)
                  &uniforms
                  (sampler :sampler-2d))
  (texture sampler (vec2 (.x uv) (- 1 (.y uv)))))

(define-shader mesh ()
  (:vertex (mesh/vert mesh-attrs))
  (:fragment (mesh/frag :vec2)))
