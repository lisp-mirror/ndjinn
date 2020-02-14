(in-package #:pyx-examples.shader)

(defun skybox/vert ((pos :vec3)
                    &uniforms
                    (model :mat4)
                    (view :mat4)
                    (proj :mat4))
  (values (.xyww (* proj view model (vec4 pos 1)))
          (- pos)))

(defun skybox/frag ((uv :vec3)
                    &uniforms
                    (sampler :sampler-cube))
  (texture sampler uv))

(define-shader skybox ()
  (:vertex (skybox/vert :vec3))
  (:fragment (skybox/frag :vec3)))
