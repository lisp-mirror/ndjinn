(in-package #:net.mfiano.lisp.pyx.shader)

(defun matcap/vertex ((mesh-attrs mesh-attrs)
                      &uniforms
                      (model :mat4)
                      (view :mat4)
                      (proj :mat4)
                      (normal-matrix :mat3))
  (with-slots (mesh/pos mesh/normal mesh/uv1) mesh-attrs
    (let ((normal (* normal-matrix mesh/normal)))
      (values (* proj view model (vec4 mesh/pos 1))
              normal))))

(defun matcap/fragment ((normal :vec3)
                        &uniforms
                        (view :mat4)
                        (sampler :sampler-2d))
  (let ((uv (+ 0.5 (* 0.5 (vec2 (* view (vec4 (normalize normal) 0)))))))
    (texture sampler (vec2 (.x uv) (- 1 (.y uv))))))

(define-shader matcap ()
  (:vertex (matcap/vertex mesh-attrs))
  (:fragment (matcap/fragment :vec3)))
