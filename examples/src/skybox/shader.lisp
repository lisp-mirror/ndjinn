(in-package #:pyx-examples.shader)

(defun skybox/vert ((mesh-attrs mesh-attrs)
                    &uniforms
                    (model :mat4)
                    (view :mat4)
                    (proj :mat4))
  (with-slots (mesh/pos) mesh-attrs
    (values (.xyww (* proj view (vec4 mesh/pos 1)))
            mesh/pos)))

(defun skybox/frag ((uv :vec3)
                    &uniforms
                    (sampler :sampler-cube))
  (texture sampler (- uv)))

(define-shader skybox ()
  (:vertex (skybox/vert mesh-attrs))
  (:fragment (skybox/frag :vec3)))
