(in-package #:net.mfiano.lisp.pyx.examples.shader)

(defun multi-pass/vertex ((pos :vec3)
                          (uv :vec2)
                          &uniforms
                          (model :mat4)
                          (view :mat4)
                          (proj :mat4))
  (values (vec4 (* (.xy pos) 2) 0 1)
          uv))

(defun multi-pass/fragment ((uv :vec2)
                            &uniforms
                            (sampler :sampler-2d))
  (let ((color (texture sampler uv)))
    (vec4 (vec3 (- 1 (.rgb color))) (.a color))))

(define-shader multi-pass ()
  (:vertex (full-quad/vertex :vec3 :vec2))
  (:fragment (multi-pass/fragment :vec2)))
