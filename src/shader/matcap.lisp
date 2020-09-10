(in-package #:net.mfiano.lisp.pyx.shader)

(defun matcap/vert ((mesh-attrs mesh-attrs)
                    &uniforms
                    (model :mat4)
                    (view :mat4)
                    (proj :mat4)
                    (normal-matrix :mat4))
  (with-slots (mesh/pos mesh/normal mesh/uv1) mesh-attrs
    (let ((normal (normalize (vec3 (* normal-matrix (vec4 mesh/normal 0))))))
      (values (* proj view model (vec4 mesh/pos 1))
              normal))))

(defun matcap/frag ((normal :vec3)
                    &uniforms
                    (view :mat4)
                    (sampler :sampler-2d))
  (let ((uv (+ 0.5 (* 0.5 (vec2 (* view (vec4 (normalize normal) 0)))))))
    (texture sampler (vec2 (.x uv) (- 1 (.y uv))))))

(define-shader matcap ()
  (:vertex (matcap/vert mesh-attrs))
  (:fragment (matcap/frag :vec3)))
