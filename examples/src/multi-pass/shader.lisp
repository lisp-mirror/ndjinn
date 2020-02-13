(in-package #:pyx-examples.shader)

(defun multi-pass/vert ((mesh-attrs mesh-attrs)
                        &uniforms
                        (model :mat4)
                        (view :mat4)
                        (proj :mat4))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (values (vec4 (* (.xy mesh/pos) 2) 0 1)
            mesh/uv1)))

(defun multi-pass/frag ((uv :vec2)
                        &uniforms
                        (sampler :sampler-2d))
  (let ((color (texture sampler (vec2 (.x uv) (- 1 (.y uv))))))
    (vec4 (vec3 (- 1 (.rgb color))) (.a color))))

(define-shader multi-pass ()
  (:vertex (multi-pass/vert mesh-attrs))
  (:fragment (multi-pass/frag :vec2)))
