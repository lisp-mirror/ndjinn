(in-package #:net.mfiano.lisp.pyx.examples.shader)

(defun matcap/vertex ((mesh-attrs mesh-attrs)
                      &uniforms
                      (model :mat4)
                      (view :mat4)
                      (proj :mat4)
                      (normal-matrix :mat3))
  (with-slots (mesh/pos mesh/normal mesh/uv1) mesh-attrs
    (let ((pos (* model (vec4 mesh/pos 1)))
          (normal (* normal-matrix mesh/normal)))
      (values (* proj view pos)
              mesh/pos
              mesh/uv1
              normal))))

(defun matcap/fragment ((pos :vec3)
                        (uv :vec2)
                        (normal :vec3)
                        &uniforms
                        (view :mat4)
                        (matcaps :sampler-2d-array)
                        (id-map :sampler-2d))
  (let* ((muv (+ 0.5 (* 0.5 (vec2 (* view (vec4 (normalize normal) 0))))))
         (id (.rgb (texture id-map (vec2 (.x uv) (- 1 (.y uv))))))
         (id (+ (.x id) (* (.y id) 2) (* (.z id) 4)))
         (material (.rgb (texture matcaps (vec3 (.x muv) (- 1 (.y muv)) id))))
         (noise (vec3 (umbra.noise:simplex-perlin (* 40 pos)))))
    (vec4 (mix material noise 0.05) 1)))

(define-shader matcap ()
  (:vertex (matcap/vertex mesh-attrs))
  (:fragment (matcap/fragment :vec3 :vec2 :vec3)))
