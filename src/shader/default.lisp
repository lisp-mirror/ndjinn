(in-package #:net.mfiano.lisp.pyx.shader)

(defun default/vertex ((pos :vec3)
                       &uniforms
                       (model :mat4)
                       (view :mat4)
                       (proj :mat4))
  (* proj view model (vec4 pos 1)))

(defun default/fragment (&uniforms
                         (color :vec3)
                         (opacity :float))
  (vec4 color opacity))

(define-shader default ()
  (:vertex (default/vertex :vec3))
  (:fragment (default/fragment)))
