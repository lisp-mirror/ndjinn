(in-package #:pyx.shader)

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

(defun quad/vert ((pos2 :vec3)
                  (uv :vec2)
                  &uniforms
                  (model :mat4)
                  (view :mat4)
                  (proj :mat4))
  (values (* proj view model (vec4 (.xy pos2) 0 1))
          uv))

(defun quad/frag ((uv :vec2)
                  &uniforms
                  (sampler :sampler-2d))
  (texture sampler uv))

(defun mesh/vert ((mesh-attrs mesh-attrs)
                  &uniforms
                  (model :mat4)
                  (view :mat4)
                  (proj :mat4))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (values (* proj view model (vec4 mesh/pos 1))
            mesh/uv1)))

(defun mesh/frag ((uv :vec2)
                  &uniforms
                  (sampler :sampler-2d))
  (texture sampler (vec2 (.x uv) (- 1 (.y uv)))))

(defun collider/vert ((mesh-attrs mesh-attrs)
                      &uniforms
                      (model :mat4)
                      (view :mat4)
                      (proj :mat4))
  (with-slots (mesh/pos) mesh-attrs
    (* proj view model (vec4 mesh/pos 1))))

(defun collider/frag (&uniforms
                      (hit-color :vec4)
                      (miss-color :vec4)
                      (contact :bool))
  (if contact
      hit-color
      miss-color))

(define-shader full-quad ()
  (:vertex (full-quad/vert :vec3 :vec2))
  (:fragment (quad/frag :vec2)))

(define-shader quad ()
  (:vertex (quad/vert :vec3 :vec2))
  (:fragment (quad/frag :vec2)))

(define-shader mesh ()
  (:vertex (mesh/vert mesh-attrs))
  (:fragment (mesh/frag :vec2)))

(define-shader collider ()
  (:vertex (collider/vert mesh-attrs))
  (:fragment (collider/frag)))
