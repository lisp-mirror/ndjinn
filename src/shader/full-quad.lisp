(in-package #:net.mfiano.lisp.pyx.shader)

(defun full-quad/vert ((pos2 :vec3)
                       (uv :vec2)
                       &uniforms
                       (model :mat4)
                       (view :mat4)
                       (proj :mat4))
  (values (vec4 (* (.xy pos2) 2) 0 1)
          uv))

(defun full-quad-no-uv/vert ((pos2 :vec3)
                             &uniforms
                             (model :mat4)
                             (view :mat4)
                             (proj :mat4))
  (vec4 (* (.xy pos2) 2) 0 1))

(define-shader full-quad ()
  (:vertex (full-quad/vert :vec3 :vec2))
  (:fragment (quad/frag :vec2)))
