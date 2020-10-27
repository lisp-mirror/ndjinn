(in-package #:net.mfiano.lisp.pyx.examples.shader)

(defun skybox/vertex ((pos :vec3)
                      &uniforms
                      (model :mat4)
                      (view :mat4)
                      (proj :mat4))
  (values (.xyww (* proj view model (vec4 pos 1)))
          (- pos)))

(defun skybox/fragment ((uv :vec3)
                        &uniforms
                        (sampler :sampler-cube))
  (texture sampler uv))

(define-shader skybox ()
  (:vertex (skybox/vertex :vec3))
  (:fragment (skybox/fragment :vec3)))
